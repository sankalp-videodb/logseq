(ns electron.task-reminder
  "Schedules renderer-validated task reminders and displays native notifications."
  (:require ["electron" :refer [Notification]]
            [electron.utils :as utils]
            [electron.window :as window]))

(def ^:private max-timeout-ms 2147483647)
(defonce ^:private *timers (atom {}))
(defonce ^:private *notifications (atom {}))

(defn- reminder-key
  [^js win task-id]
  [(.-id win) (str task-id)])

(defn- usable-window?
  [^js win]
  (and win
       (not (.isDestroyed win))
       (not (.. win -webContents isDestroyed))))

(defn cancel!
  [^js win task-id]
  (let [key (reminder-key win task-id)]
    (when-let [timer-id (get-in @*timers [key :timer-id])]
      (js/clearTimeout timer-id))
    (swap! *timers dissoc key)))

(defn cancel-all!
  [^js win]
  (let [window-id (.-id win)]
    (doseq [[[timer-window-id task-id] _] @*timers
            :when (= window-id timer-window-id)]
      (cancel! win task-id))))

(declare arm-reminder!)

(defn- reminder-due!
  [^js win {:keys [task-id] :as reminder}]
  (swap! *timers dissoc (reminder-key win task-id))
  (when (usable-window? win)
    ;; The renderer validates that the task still exists, is active, and has the
    ;; same scheduled timestamp before asking the main process to notify.
    (utils/send-to-renderer win :task-reminder-due reminder)))

(defn- arm-reminder!
  [^js win {:keys [task-id scheduled-at] :as reminder}]
  (let [remaining (- scheduled-at (js/Date.now))]
    (when (pos? remaining)
      (let [delay (min remaining max-timeout-ms)
            timer-id (js/setTimeout
                      (fn []
                        (if (> (- scheduled-at (js/Date.now)) 1000)
                          (arm-reminder! win reminder)
                          (reminder-due! win reminder)))
                      delay)]
        (swap! *timers assoc (reminder-key win task-id)
               {:timer-id timer-id :reminder reminder})))))

(defn schedule!
  [^js win {:keys [task-id scheduled-at] :as reminder}]
  (cancel! win task-id)
  (when (and (number? scheduled-at)
             (> scheduled-at (js/Date.now)))
    (arm-reminder! win reminder))
  nil)

(defn replace-all!
  [^js win reminders]
  (cancel-all! win)
  (run! #(schedule! win %) reminders)
  nil)

(defn show!
  [^js win {:keys [task-id title body] :as reminder}]
  (when (and (usable-window? win)
             (.isSupported Notification))
    (let [key (reminder-key win task-id)
          notification (Notification. (clj->js {:title title
                                                :body body
                                                :silent false}))]
      (when-let [previous (get @*notifications key)]
        (.close previous))
      (swap! *notifications assoc key notification)
      (.on notification "click"
           (fn []
             (when (usable-window? win)
               (window/switch-to-window! win)
               (utils/send-to-renderer win :task-reminder-clicked reminder))))
      (.on notification "close"
           #(swap! *notifications dissoc key))
      (.show notification)))
  nil)
