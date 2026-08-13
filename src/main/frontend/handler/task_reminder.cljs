(ns frontend.handler.task-reminder
  "Coordinates task schedules with Electron's native notification service."
  (:require [cljs-bean.core :as bean]
            [clojure.string :as string]
            [electron.ipc :as ipc]
            [frontend.context.i18n :refer [t]]
            [frontend.db.async :as db-async]
            [frontend.format.mldoc :as mldoc]
            [frontend.handler.route :as route-handler]
            [frontend.state :as state]
            [frontend.util :as util]
            [lambdaisland.glogi :as log]
            [promesa.core :as p]))

(def ^:private inactive-statuses
  #{:logseq.property/status.done :logseq.property/status.canceled})

(defonce ^:private *listeners-installed? (atom false))

(defn- task-tagged?
  [block]
  (some #(= :logseq.class/Task (:db/ident %)) (:block/tags block)))

(defn- active-task?
  [block]
  (and block
       (task-tagged? block)
       (not (contains? inactive-statuses
                       (:db/ident (:logseq.property/status block))))))

(defn schedule!
  [block scheduled-at]
  (when (and (util/electron?) (number? scheduled-at))
    (ipc/ipc :task-reminder/schedule
             {:task-id (str (:block/uuid block))
              :graph (state/get-current-repo)
              :scheduled-at scheduled-at})))

(defn cancel!
  [block]
  (when (util/electron?)
    (ipc/ipc :task-reminder/cancel {:task-id (str (:block/uuid block))})))

(defn- <future-task-reminders
  [repo]
  (p/let [rows (db-async/<q
                repo
                {:transact-db? false}
                '[:find (pull ?task [:block/uuid
                                     :block/title
                                     :logseq.property/scheduled
                                     {:block/tags [:db/ident]}
                                     {:logseq.property/status [:db/ident]}])
                  :where
                  [?task :block/tags :logseq.class/Task]
                  [?task :logseq.property/scheduled]])
          now (js/Date.now)]
    (->> rows
         (map first)
         (filter active-task?)
         (keep (fn [block]
                 (let [scheduled-at (:logseq.property/scheduled block)]
                   (when (and (number? scheduled-at) (> scheduled-at now))
                     {:task-id (str (:block/uuid block))
                      :graph repo
                      :scheduled-at scheduled-at}))))
         vec)))

(defn register-future-reminders!
  [repo]
  (when (util/electron?)
    (-> (p/let [reminders (<future-task-reminders repo)]
          (ipc/ipc :task-reminder/replace-all reminders))
        (p/catch #(log/error :task-reminder/register-error %)))))

(defn- notification-body
  [block]
  (let [title (some-> (:block/title block) mldoc/plain->text string/trim)]
    (if (seq title) title (t :block.task-config/untitled-task))))

(defn- handle-due!
  [payload]
  (let [{:keys [task-id graph scheduled-at]} (bean/->clj payload :keywordize-keys true)]
    (when (= graph (state/get-current-repo))
      (-> (p/let [block (db-async/<get-block graph task-id {:children? false})]
            (when (and (active-task? block)
                       (= scheduled-at (:logseq.property/scheduled block)))
              (ipc/ipc :task-reminder/show
                       {:task-id task-id
                        :graph graph
                        :scheduled-at scheduled-at
                        :title (t :block.task-config/notification-title)
                        :body (notification-body block)})))
          (p/catch #(log/error :task-reminder/due-validation-error %))))))

(defn- handle-clicked!
  [payload]
  (let [{:keys [task-id graph]} (bean/->clj payload :keywordize-keys true)]
    (when (= graph (state/get-current-repo))
      (route-handler/redirect-to-page! task-id))))

(defn- install-listeners!
  []
  (when (and (util/electron?)
             (compare-and-set! *listeners-installed? false true))
    (.on js/window.apis "task-reminder-due" handle-due!)
    (.on js/window.apis "task-reminder-clicked" handle-clicked!)))

(defn start!
  [repo]
  (install-listeners!)
  (register-future-reminders! repo))
