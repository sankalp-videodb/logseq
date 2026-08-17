(ns frontend.handler.goals
  "Goal persistence, journal check-in generation, and progress mutations."
  (:require [clojure.string :as string]
            [frontend.date :as date]
            [frontend.db.async :as db-async]
            [frontend.handler.db-based.property :as db-property-handler]
            [frontend.handler.editor :as editor-handler]
            [frontend.handler.page :as page-handler]
            [frontend.handler.task-reminder :as task-reminder-handler]
            [frontend.state :as state]
            [frontend.util.goals :as goals]
            [lambdaisland.glogi :as log]
            [logseq.common.util.date-time :as date-time-util]
            [logseq.common.uuid :as common-uuid]
            [promesa.core :as p]))

(defonce ^:private *backfill-in-flight (atom #{}))

(def ^:private active-state "Active")

(def ^:private kind-value
  {goals/daily-kind "Daily check-in"
   goals/progress-kind "Weekly progress"
   goals/pause-kind "Pause"
   goals/resume-kind "Resume"
   goals/archive-kind "Archive"})

(defn- <goal-data
  [repo]
  (p/let [goal-rows (db-async/<q
                      repo {:transact-db? false}
                      '[:find (pull ?goal [:db/id :block/uuid :block/title :block/created-at
                                           {:logseq.property/description [:block/title]}
                                           :logseq.property.goal/weekly-target
                                           :logseq.property.goal/weekly-unit
                                           :logseq.property.goal/daily-check-in
                                           :logseq.property.goal/check-in-days
                                           :logseq.property.goal/reminder-minutes
                                           :logseq.property.goal/start-day
                                           {:logseq.property.goal/state [:db/ident]}])
                        :where [?goal :block/tags :logseq.class/Goal]])
          record-rows (db-async/<q
                        repo {:transact-db? false}
                        '[:find (pull ?record [:db/id :block/uuid :block/title :block/created-at
                                               :logseq.property.goal/record-day
                                               :logseq.property.goal/value
                                               :logseq.property/scheduled
                                               {:logseq.property.goal/ref [:db/id :block/uuid]}
                                               {:logseq.property.goal/record-kind [:db/ident]}
                                               {:logseq.property/status [:db/ident]}])
                          :where
                          [?record :logseq.property.goal/ref]
                          [?record :logseq.property.goal/record-kind]])]
    {:goals (mapv (fn [row]
                    (update (first row) :logseq.property/description
                            #(if (map? %) (:block/title %) %)))
                  goal-rows)
     :records (mapv first record-rows)}))

(defn- <ensure-journal-page
  [journal-day]
  (let [repo (state/get-current-repo)]
    (p/let [existing (db-async/<get-journal-page-by-day repo journal-day)]
      (if existing
        existing
        (let [title (date-time-util/int->journal-title journal-day
                                                       (state/get-date-formatter))]
          (p/let [_ (page-handler/<create! title {:redirect? false
                                                  :split-namespace? false})]
            (db-async/<get-journal-page-by-day repo journal-day)))))))

(defn- scheduled-at
  [journal-day reminder-minutes]
  (when (and (integer? journal-day) (integer? reminder-minutes))
    (let [date (date-time-util/int->local-date journal-day)]
      (.setHours date (quot reminder-minutes 60) (mod reminder-minutes 60) 0 0)
      (.getTime date))))

(defn- <set-record-kind!
  [record kind]
  (db-property-handler/batch-set-property-closed-value!
   [(:block/uuid record)]
   :logseq.property.goal/record-kind
   (get kind-value kind)))

(defn- <create-record!
  [goal journal-day kind title {:keys [value task-status reminder-minutes]}]
  (p/let [page (<ensure-journal-page journal-day)
          _ (when-not page
              (throw (ex-info "Journal page was not created" {:journal-day journal-day})))
          deterministic-daily? (= kind goals/daily-kind)
          record-uuid (if deterministic-daily?
                        (common-uuid/gen-uuid :builtin-block-uuid
                                              (str "goal-daily-record:" (:block/uuid goal) ":" journal-day))
                        (random-uuid))
          tags (if task-status
                 #{:logseq.class/Task}
                 #{:logseq.class/Goal-record})
          record (editor-handler/api-insert-new-block!
                  title
                  {:page (:block/uuid page)
                   :properties {:block/tags tags}
                   :custom-uuid record-uuid
                   :edit-block? false
                   :end? true})
          properties (cond-> {:logseq.property.goal/ref (:db/id goal)
                              :logseq.property.goal/record-day journal-day}
                       value (assoc :logseq.property.goal/value value)
                       reminder-minutes (assoc :logseq.property/scheduled
                                               (scheduled-at journal-day reminder-minutes)))
          _ (db-property-handler/set-block-properties! (:block/uuid record) properties)
          _ (<set-record-kind! record kind)
          _ (when task-status
              (db-property-handler/batch-set-property-closed-value!
               [(:block/uuid record)] :logseq.property/status task-status))
          scheduled (when reminder-minutes (scheduled-at journal-day reminder-minutes))
          _ (when (and task-status scheduled (> scheduled (js/Date.now)))
              (task-reminder-handler/schedule! record scheduled))]
    record))

(defn- <create-daily-check-in!
  [goal journal-day today-day]
  (<create-record! goal journal-day goals/daily-kind
                   (:logseq.property.goal/daily-check-in goal)
                   {:task-status (if (< journal-day today-day) "Canceled" "Todo")
                    :reminder-minutes (when (= journal-day today-day)
                                       (:logseq.property.goal/reminder-minutes goal))}))

(declare set-check-in-status!)

(defn ensure-check-ins!
  ([] (ensure-check-ins! (state/get-current-repo)))
  ([repo]
   (let [today-day (date/today-journal-day)
         run-key [repo today-day]]
     (when (and repo (not (contains? @*backfill-in-flight run-key)))
       (swap! *backfill-in-flight conj run-key)
       (-> (p/let [{:keys [goals records]} (<goal-data repo)
                   missing (for [goal goals
                                 :let [goal-records (goals/records-for-goal records goal)]
                                 journal-day (goals/missing-check-in-days goal goal-records today-day)]
                             [goal journal-day])
                   stale-todos (filter #(and (= goals/daily-kind (goals/record-kind %))
                                             (< (:logseq.property.goal/record-day %) today-day)
                                             (= :logseq.property/status.todo
                                                (goals/record-status %)))
                                       records)
                   _ (reduce (fn [result record]
                               (p/then result
                                       (fn [_]
                                         (set-check-in-status! record :missed))))
                             (p/resolved nil)
                             stale-todos)]
             (reduce (fn [result [goal journal-day]]
                       (p/then result
                               (fn [_]
                                 (<create-daily-check-in! goal journal-day today-day))))
                     (p/resolved nil)
                     missing))
           (p/catch #(log/error :goals/backfill-error %))
           (p/finally #(swap! *backfill-in-flight disj run-key)))))))

(defn create-goal!
  [{:keys [title daily-check-in check-in-days reminder-minutes]}]
  (when (string/blank? title)
    (throw (ex-info "Goal title is required" {})))
  (when (string/blank? daily-check-in)
    (throw (ex-info "Check-in task title is required" {})))
  (when-not (pos? (or check-in-days 0))
    (throw (ex-info "At least one task day is required" {})))
  (let [repo (state/get-current-repo)
        today-day (date/today-journal-day)]
    (p/let [goal-class (state/<invoke-db-worker :thread-api/pull repo [:block/uuid]
                                                :logseq.class/Goal)
            goal (editor-handler/api-insert-new-block!
                  (string/trim title)
                  {:page (:block/uuid goal-class)
                   :properties {:block/tags #{:logseq.class/Goal}}
                   :edit-block? false
                   :end? true})
            properties (cond-> {:logseq.property/description ""
                                :logseq.property.goal/weekly-target 1
                                :logseq.property.goal/weekly-unit "check-in"
                                :logseq.property.goal/start-day today-day
                                :logseq.property.goal/daily-check-in (string/trim daily-check-in)
                                :logseq.property.goal/check-in-days check-in-days}
                         (integer? reminder-minutes)
                         (assoc :logseq.property.goal/reminder-minutes reminder-minutes))
            _ (db-property-handler/set-block-properties! (:block/uuid goal) properties)
            _ (db-property-handler/batch-set-property-closed-value!
               [(:block/uuid goal)] :logseq.property.goal/state active-state)
            loaded (db-async/<get-block repo (:block/uuid goal) {:children? false})
            _ (when (goals/scheduled-on-day? loaded today-day)
                (<create-daily-check-in! loaded today-day today-day))]
      loaded)))

(defn- <sync-today-task!
  [goal task-title reminder-minutes]
  (let [repo (state/get-current-repo)
        today-day (date/today-journal-day)]
    (p/let [{:keys [records]} (<goal-data repo)
            record (some #(when (and (= goals/daily-kind (goals/record-kind %))
                                     (= today-day (:logseq.property.goal/record-day %)))
                            %)
                         (goals/records-for-goal records goal))]
      (when record
        (p/do!
         (when-not (string/blank? task-title)
           (db-property-handler/set-block-property!
            (:block/uuid record) :block/title (string/trim task-title)))
         (if (integer? reminder-minutes)
           (let [scheduled (scheduled-at today-day reminder-minutes)]
             (p/do!
              (db-property-handler/set-block-property!
               (:block/uuid record) :logseq.property/scheduled scheduled)
              (if (> scheduled (js/Date.now))
                (task-reminder-handler/schedule! record scheduled)
                (task-reminder-handler/cancel! record))))
           (p/do!
            (db-property-handler/remove-block-property!
             (:block/uuid record) :logseq.property/scheduled)
            (task-reminder-handler/cancel! record))))))))

(defn update-goal!
  [goal {:keys [title daily-check-in check-in-days reminder-minutes]}]
  (when (string/blank? title)
    (throw (ex-info "Goal title is required" {})))
  (when (string/blank? daily-check-in)
    (throw (ex-info "Check-in task title is required" {})))
  (when-not (pos? (or check-in-days 0))
    (throw (ex-info "At least one task day is required" {})))
  (let [uuid (:block/uuid goal)
        properties {:block/title (string/trim title)
                    :logseq.property.goal/daily-check-in (string/trim daily-check-in)
                    :logseq.property.goal/check-in-days check-in-days}]
    (p/do!
     (db-property-handler/set-block-properties! uuid properties)
     (if (integer? reminder-minutes)
       (db-property-handler/set-block-property! uuid :logseq.property.goal/reminder-minutes reminder-minutes)
       (db-property-handler/remove-block-property! uuid :logseq.property.goal/reminder-minutes))
     (<sync-today-task! goal daily-check-in reminder-minutes)
     (ensure-check-ins!))))

(defn set-check-in-status!
  [record status]
  (let [value (case status
                :completed "Done"
                :missed "Canceled"
                (throw (ex-info "Unsupported goal check-in status" {:status status})))]
    (db-property-handler/batch-set-property-closed-value!
     [(:block/uuid record)] :logseq.property/status value)))

(defn delete-goal!
  [goal]
  (let [repo (state/get-current-repo)]
    (p/let [{:keys [records]} (<goal-data repo)
            goal-records (goals/records-for-goal records goal)]
      (doseq [record goal-records]
        (editor-handler/delete-block-aux! record))
      (editor-handler/delete-block-aux! goal))))

(defn start!
  [repo]
  (ensure-check-ins! repo))
