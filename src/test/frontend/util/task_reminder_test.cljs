(ns frontend.util.task-reminder-test
  (:require [cljs.test :refer [deftest is testing]]
            [frontend.util.task-reminder :as task-reminder]))

(defn- local-date
  [year month day hour minute]
  (js/Date. year (dec month) day hour minute 0 0))

(deftest relative-reminders
  (let [now (local-date 2026 8 13 14 30)
        times (task-reminder/preset-times now)]
    (testing "relative reminders preserve the exact current minute"
      (is (= (+ (.getTime now) (* 60 60 1000)) (:in-one-hour times)))
      (is (= (+ (.getTime now) (* 3 60 60 1000)) (:in-three-hours times))))))

(deftest fixed-local-reminders
  (let [now (local-date 2026 8 13 14 30) ; Thursday
        times (task-reminder/preset-times now)]
    (is (= (.getTime (local-date 2026 8 13 22 0)) (:today-at-22 times)))
    (is (= (.getTime (local-date 2026 8 14 10 0)) (:tomorrow-at-10 times)))
    (is (= (.getTime (local-date 2026 8 15 10 0)) (:saturday-at-10 times)))
    (is (= (.getTime (local-date 2026 8 17 10 0)) (:monday-at-10 times)))))

(deftest passed-and-same-weekday-reminders
  (testing "today at 22:00 is unavailable after it has passed"
    (is (nil? (:today-at-22 (task-reminder/preset-times (local-date 2026 8 13 22 1))))))
  (testing "the current weekday qualifies while its 10:00 time is still future"
    (is (= (.getTime (local-date 2026 8 15 10 0))
           (:saturday-at-10 (task-reminder/preset-times (local-date 2026 8 15 9 0))))))
  (testing "the next week's occurrence is used once today's time has passed"
    (is (= (.getTime (local-date 2026 8 22 10 0))
           (:saturday-at-10 (task-reminder/preset-times (local-date 2026 8 15 10 1)))))))
