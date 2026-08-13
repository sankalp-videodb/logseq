(ns frontend.util.task-reminder)

(def ^:private hour-ms (* 60 60 1000))

(defn- local-time
  [^js now days-ahead hour]
  (js/Date. (.getFullYear now)
            (.getMonth now)
            (+ (.getDate now) days-ahead)
            hour
            0
            0
            0))

(defn- next-weekday-time
  [^js now target-day hour]
  (let [days-ahead (mod (- target-day (.getDay now)) 7)
        candidate (local-time now days-ahead hour)]
    (.getTime (if (> (.getTime candidate) (.getTime now))
                candidate
                (local-time now (+ days-ahead 7) hour)))))

(defn preset-times
  "Return reminder timestamps in local time. Today at 22:00 is nil once it has passed."
  [^js now]
  (let [now-ms (.getTime now)
        today-at-22 (local-time now 0 22)]
    {:in-one-hour (+ now-ms hour-ms)
     :in-three-hours (+ now-ms (* 3 hour-ms))
     :today-at-22 (when (> (.getTime today-at-22) now-ms)
                    (.getTime today-at-22))
     :tomorrow-at-10 (.getTime (local-time now 1 10))
     :saturday-at-10 (next-weekday-time now 6 10)
     :monday-at-10 (next-weekday-time now 1 10)}))
