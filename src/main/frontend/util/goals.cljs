(ns frontend.util.goals
  "Pure date, progress, and streak calculations for goals."
  (:require [clojure.string :as string]
            [logseq.common.util.date-time :as date-time-util]))

(def daily-kind :logseq.property.goal/record-kind.daily)
(def progress-kind :logseq.property.goal/record-kind.progress)
(def pause-kind :logseq.property.goal/record-kind.pause)
(def resume-kind :logseq.property.goal/record-kind.resume)
(def archive-kind :logseq.property.goal/record-kind.archive)

(def done-status :logseq.property/status.done)
(def missed-status :logseq.property/status.canceled)

(def all-weekdays-mask 127)

(defn numeric-value
  "Unwrap legacy :number property entities while graphs migrate to raw numbers."
  [value]
  (cond
    (number? value) value
    (map? value) (:logseq.property/value value)
    :else nil))

(defn check-in-days-mask
  [goal]
  (or (numeric-value (:logseq.property.goal/check-in-days goal))
      all-weekdays-mask))

(defn scheduled-on-day?
  [goal journal-day]
  (when-not (string/blank? (:logseq.property.goal/daily-check-in goal))
    (let [weekday (.getDay (date-time-util/int->local-date journal-day))]
      (not (zero? (bit-and (check-in-days-mask goal)
                           (bit-shift-left 1 weekday)))))))

(defn entity-ident
  [value]
  (cond
    (keyword? value) value
    (map? value) (:db/ident value)
    :else nil))

(defn entity-id
  [value]
  (cond
    (integer? value) value
    (map? value) (:db/id value)
    :else nil))

(defn add-days
  [journal-day amount]
  (let [date (date-time-util/int->local-date journal-day)]
    (.setDate date (+ (.getDate date) amount))
    (date-time-util/date->int date)))

(defn days-inclusive
  [start-day end-day]
  (when (and (integer? start-day)
             (integer? end-day)
             (<= start-day end-day))
    (loop [day start-day
           result []]
      (if (> day end-day)
        result
        (recur (add-days day 1) (conj result day))))))

(defn week-start
  "Return the Monday journal day for journal-day."
  [journal-day]
  (let [date (date-time-util/int->local-date journal-day)
        weekday (.getDay date)
        days-since-monday (mod (+ weekday 6) 7)]
    (add-days journal-day (- days-since-monday))))

(defn week-end
  [journal-day]
  (add-days (week-start journal-day) 6))

(defn record-kind
  [record]
  (entity-ident (:logseq.property.goal/record-kind record)))

(defn record-status
  [record]
  (entity-ident (:logseq.property/status record)))

(defn record-goal-id
  [record]
  (entity-id (:logseq.property.goal/ref record)))

(defn records-for-goal
  [records goal]
  (let [goal-id (:db/id goal)]
    (filterv #(= goal-id (record-goal-id %)) records)))

(defn- control-kind?
  [kind]
  (contains? #{pause-kind resume-kind archive-kind} kind))

(defn active-on-day?
  "Whether a goal is active on journal-day according to its immutable control events."
  [goal records journal-day]
  (let [start-day (:logseq.property.goal/start-day goal)
        latest-control (->> records
                            (filter #(and (control-kind? (record-kind %))
                                          (<= (:logseq.property.goal/record-day %) journal-day)))
                            (sort-by (juxt :logseq.property.goal/record-day :block/created-at))
                            last)
        kind (some-> latest-control record-kind)]
    (and (integer? start-day)
         (<= start-day journal-day)
         (not (contains? #{pause-kind archive-kind} kind)))))

(defn daily-record-by-day
  [records]
  (->> records
       (filter #(= daily-kind (record-kind %)))
       (sort-by :block/created-at)
       (reduce (fn [result record]
                 (assoc result (:logseq.property.goal/record-day record) record))
               {})))

(defn missing-check-in-days
  [goal records today-day]
  (let [record-by-day (daily-record-by-day records)]
    (if (not (integer? (:logseq.property.goal/start-day goal)))
      []
      (->> (days-inclusive (:logseq.property.goal/start-day goal) today-day)
           (filter #(and (scheduled-on-day? goal %)
                         (active-on-day? goal records %)
                         (nil? (get record-by-day %))))
           vec))))

(defn streak-summary
  "Calculate current and longest streaks. Today remains neutral until explicitly
  completed or missed. Paused days are removed from the sequence."
  [goal records today-day]
  (let [record-by-day (daily-record-by-day records)
        today-status (some-> (get record-by-day today-day) record-status)
        evaluation-end (if (contains? #{done-status missed-status} today-status)
                         today-day
                         (add-days today-day -1))
        days (->> (days-inclusive (:logseq.property.goal/start-day goal) evaluation-end)
                  (filter #(and (scheduled-on-day? goal %)
                                (active-on-day? goal records %))))]
    (reduce (fn [{:keys [current longest]} day]
              (let [completed? (= done-status (some-> (get record-by-day day) record-status))
                    current' (if completed? (inc current) 0)]
                {:current current'
                 :longest (max longest current')}))
            {:current 0 :longest 0}
            days)))

(defn weekly-progress
  [records journal-day]
  (let [start (week-start journal-day)
        end (week-end journal-day)]
    (->> records
         (filter #(and (= progress-kind (record-kind %))
                       (<= start (:logseq.property.goal/record-day %) end)))
         (map #(or (numeric-value (:logseq.property.goal/value %)) 1))
         (reduce + 0))))

(defn weekly-history
  [records]
  (->> records
       (filter #(= progress-kind (record-kind %)))
       (group-by #(week-start (:logseq.property.goal/record-day %)))
       (map (fn [[start items]]
              {:week-start start
               :week-end (week-end start)
               :value (reduce + 0 (map #(or (numeric-value (:logseq.property.goal/value %)) 1) items))}))
       (sort-by :week-start >)
       vec))

(defn goal-summary
  [goal all-records today-day]
  (let [records (records-for-goal all-records goal)
        record-by-day (daily-record-by-day records)
        streaks (if (string/blank? (:logseq.property.goal/daily-check-in goal))
                  nil
                  (streak-summary goal records today-day))]
    {:records records
     :record-by-day record-by-day
     :today-scheduled? (boolean (scheduled-on-day? goal today-day))
     :today-record (get record-by-day today-day)
     :missing-days (missing-check-in-days goal records today-day)
     :streaks streaks
     :weekly-progress (weekly-progress records today-day)
     :weekly-history (weekly-history records)}))
