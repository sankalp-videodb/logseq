(ns frontend.util.goals-test
  (:require [cljs.test :refer [deftest is testing]]
            [frontend.util.goals :as goals]))

(defn- goal
  ([] (goal {}))
  ([overrides]
   (merge {:db/id 1
           :logseq.property.goal/start-day 20260810
           :logseq.property.goal/daily-check-in "Read something technical"
           :logseq.property.goal/weekly-target 1}
          overrides)))

(defn- record
  [day kind & [{:keys [status value created-at]
                :or {created-at day}}]]
  (cond-> {:block/created-at created-at
           :logseq.property.goal/ref {:db/id 1}
           :logseq.property.goal/record-day day
           :logseq.property.goal/record-kind {:db/ident kind}}
    status (assoc :logseq.property/status {:db/ident status})
    value (assoc :logseq.property.goal/value value)))

(deftest monday-to-sunday-week-test
  (testing "week boundaries use Monday through Sunday"
    (is (= 20260810 (goals/week-start 20260810)))
    (is (= 20260810 (goals/week-start 20260816)))
    (is (= 20260816 (goals/week-end 20260810)))
    (is (= 20260817 (goals/week-start 20260817)))))

(deftest missing-check-ins-are-backfilled-test
  (let [records [(record 20260810 goals/daily-kind
                         {:status goals/done-status})]]
    (is (= [20260811 20260812]
           (goals/missing-check-in-days (goal) records 20260812)))))

(deftest selected-weekdays-drive-tasks-and-streaks-test
  (let [monday-and-wednesday (bit-or (bit-shift-left 1 1)
                                     (bit-shift-left 1 3))
        scheduled-goal (goal {:logseq.property.goal/check-in-days monday-and-wednesday})
        monday-done [(record 20260810 goals/daily-kind {:status goals/done-status})]]
    (is (true? (goals/scheduled-on-day? scheduled-goal 20260810)))
    (is (not (goals/scheduled-on-day? scheduled-goal 20260811)))
    (is (= [20260812]
           (goals/missing-check-in-days scheduled-goal monday-done 20260812)))
    (is (= {:current 1 :longest 1}
           (goals/streak-summary scheduled-goal monday-done 20260812)))
    (is (= {:current 2 :longest 2}
           (goals/streak-summary
            scheduled-goal
            (conj monday-done
                  (record 20260812 goals/daily-kind {:status goals/done-status}))
            20260812)))))

(deftest legacy-number-values-are-unwrapped-test
  (is (= 5 (goals/numeric-value {:db/id 600 :logseq.property/value 5})))
  (is (= 5 (goals/numeric-value 5))))

(deftest current-day-is-neutral-until-resolved-test
  (let [records [(record 20260810 goals/daily-kind {:status goals/done-status})
                 (record 20260811 goals/daily-kind {:status goals/done-status})
                 (record 20260812 goals/daily-kind {:status :logseq.property/status.todo})]]
    (is (= {:current 2 :longest 2}
           (goals/streak-summary (goal) records 20260812)))
    (is (= {:current 0 :longest 2}
           (goals/streak-summary
            (goal)
            (assoc records 2 (record 20260812 goals/daily-kind
                                     {:status goals/missed-status}))
            20260812)))))

(deftest paused-days-do-not-break-streak-test
  (let [records [(record 20260810 goals/daily-kind {:status goals/done-status})
                 (record 20260811 goals/pause-kind)
                 (record 20260813 goals/resume-kind)
                 (record 20260813 goals/daily-kind {:status goals/done-status})]]
    (is (= [] (goals/missing-check-in-days (goal) records 20260813)))
    (is (= {:current 2 :longest 2}
           (goals/streak-summary (goal) records 20260813)))))

(deftest weekly-progress-and-history-test
  (let [records [(record 20260810 goals/progress-kind {:value 1})
                 (record 20260812 goals/progress-kind {:value 2})
                 (record 20260817 goals/progress-kind {:value 1})]]
    (is (= 3 (goals/weekly-progress records 20260816)))
    (is (= [{:week-start 20260817 :week-end 20260823 :value 1}
            {:week-start 20260810 :week-end 20260816 :value 3}]
           (goals/weekly-history records)))))
