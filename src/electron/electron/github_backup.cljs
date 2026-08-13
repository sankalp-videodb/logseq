(ns electron.github-backup
  "Uploads completed local graph backups to a rotating GitHub Release."
  (:require ["child_process" :as child-process]
            ["fs-extra" :as fs]
            ["os" :as os]
            ["path" :as node-path]
            [clojure.string :as string]
            [lambdaisland.glogi :as log]
            [promesa.core :as p]))

(def ^:private config-file-name "github-backup-repository")
(def ^:private release-tag "logseq-backups")
(def ^:private keep-assets 12)

(defonce ^:private *upload-queue (atom (p/resolved nil)))

(defn- configured-repository
  []
  (let [config-path (node-path/join (.homedir os) ".logseq" config-file-name)]
    (when (fs/existsSync config-path)
      (some-> (fs/readFileSync config-path "utf8")
              string/trim
              not-empty))))

(defn- enabled?
  []
  ;; Upload only from Electron itself, never from Node-based tests or CLI tools.
  (and (some? (.-electron js/process.versions))
       (not= "test" (.-NODE_ENV js/process.env))))

(defn- gh-command
  []
  (or (some #(when (fs/existsSync %) %)
            ["/opt/homebrew/bin/gh" "/usr/local/bin/gh"])
      "gh"))

(defn- run-gh!
  [args]
  (js/Promise.
   (fn [resolve reject]
     (let [stdout (atom "")
           stderr (atom "")
           proc (.spawn child-process
                        (gh-command)
                        (clj->js args)
                        #js {:stdio "pipe"})]
       (.on (.-stdout proc) "data" #(swap! stdout str (.toString %)))
       (.on (.-stderr proc) "data" #(swap! stderr str (.toString %)))
       (.once proc "error" reject)
       (.once proc "close"
              (fn [code]
                (if (zero? code)
                  (resolve @stdout)
                  (reject
                   (ex-info "GitHub CLI command failed"
                            {:args args
                             :exit-code code
                             :stderr (string/trim @stderr)})))))))))

(defn- ensure-release!
  [repo]
  (-> (run-gh! ["release" "view" release-tag "--repo" repo])
      (p/catch
       (fn [_]
         (run-gh! ["release" "create" release-tag
                   "--repo" repo
                   "--title" "Logseq backups"
                   "--notes" "Automatic rotating Logseq database backups."])))))

(defn- release-assets!
  [repo]
  (-> (run-gh! ["api" (str "repos/" repo "/releases/tags/" release-tag)
                "--jq" ".assets | sort_by(.created_at) | map({id: .id, name: .name, size: .size}) | @json"])
      (p/then #(js->clj (js/JSON.parse %) :keywordize-keys true))))

(defn- prune-assets!
  [repo assets]
  (p/all
   (map (fn [{:keys [id]}]
          (run-gh! ["api" "--method" "DELETE"
                    (str "repos/" repo "/releases/assets/" id)]))
        (drop-last keep-assets assets))))

(defn- upload-backup!
  [repo {:keys [backup-name path]}]
  (let [asset-name (str (node-path/basename backup-name) ".sqlite")
        upload-dir (fs/mkdtempSync (node-path/join (.tmpdir os) "logseq-github-backup-"))
        upload-path (node-path/join upload-dir asset-name)]
    (fs/copyFileSync path upload-path)
    (-> (p/let [_ (ensure-release! repo)
                _ (run-gh! ["release" "upload" release-tag upload-path
                            "--repo" repo])
                assets (release-assets! repo)
                uploaded (some #(when (= asset-name (:name %)) %) assets)
                _ (when-not (and uploaded
                                 (= (.-size (fs/statSync path)) (:size uploaded)))
                    (throw (ex-info "Uploaded GitHub backup could not be verified"
                                    {:asset asset-name})))
                _ (prune-assets! repo assets)]
          (log/info :electron/github-backup-uploaded
                    {:repo repo :asset asset-name})
          {:repo repo :asset asset-name})
        (p/finally #(fs/rmSync upload-dir #js {:recursive true :force true})))))

(defn enqueue-upload!
  "Queue a completed local backup for upload. Upload failures never affect the
  local backup and are logged for diagnosis."
  [{:keys [created? path backup-name] :as backup}]
  (when-let [repo (and (enabled?)
                       created?
                       (seq path)
                       (seq backup-name)
                       (configured-repository))]
    (swap! *upload-queue
           (fn [queue]
             (-> queue
                 (p/catch (fn [_] nil))
                 (p/then (fn [_] (upload-backup! repo backup)))
                 (p/catch (fn [error]
                            (log/error :electron/github-backup-failed
                                       {:backup-name backup-name
                                        :error error})
                            nil))))))
  nil)
