(ns frontend.components.goals
  (:require [clojure.string :as string]
            [frontend.context.i18n :as i18n :refer [t]]
            [frontend.date :as date]
            [frontend.db.hooks :as db-hooks]
            [frontend.handler.goals :as goals-handler]
            [frontend.ui :as ui]
            [frontend.util.goals :as goals]
            [io.factorhouse.hsx.core :as hsx]
            [logseq.common.util.date-time :as date-time-util]
            [logseq.shui.hooks :as hooks]
            [logseq.shui.ui :as shui]
            [promesa.core :as p]))

(def ^:private weekdays [1 2 3 4 5 6 0])

(defn- input-value [event]
  (.. event -target -value))

(defn- time->minutes [value]
  (when-not (string/blank? value)
    (let [[hours minutes] (map parse-long (string/split value #":"))]
      (when (and (integer? hours) (integer? minutes))
        (+ (* hours 60) minutes)))))

(defn- minutes->time [minutes]
  (when (integer? minutes)
    (str (when (< (quot minutes 60) 10) "0")
         (quot minutes 60) ":"
         (when (< (mod minutes 60) 10) "0")
         (mod minutes 60))))

(defn- weekday-label [day]
  (case day
    1 (t :goal.form/monday)
    2 (t :goal.form/tuesday)
    3 (t :goal.form/wednesday)
    4 (t :goal.form/thursday)
    5 (t :goal.form/friday)
    6 (t :goal.form/saturday)
    0 (t :goal.form/sunday)))

(defn- mask->weekdays [days-mask]
  (->> weekdays
       (filter #(not (zero? (bit-and days-mask (bit-shift-left 1 %)))))
       set))

(defn- weekdays->mask [days]
  (reduce (fn [days-mask day]
            (bit-or days-mask (bit-shift-left 1 day)))
          0
          days))

(defn- form-value [goal]
  {:title (or (:block/title goal) "")
   :task-title (or (:logseq.property.goal/daily-check-in goal) "")
   :check-in-days (mask->weekdays (goals/check-in-days-mask goal))
   :reminder (or (minutes->time
                  (goals/numeric-value (:logseq.property.goal/reminder-minutes goal))) "")})

(defn- save-payload [value]
  {:title (:title value)
   :daily-check-in (:task-title value)
   :check-in-days (weekdays->mask (:check-in-days value))
   :reminder-minutes (time->minutes (:reminder value))})

(hsx/defc goal-form
  [{:keys [goal on-saved on-cancel]}]
  (let [[tab set-tab!] (hooks/use-state :goal)
        [value set-value!] (hooks/use-state (form-value goal))
        [saving? set-saving!] (hooks/use-state false)
        [error set-error!] (hooks/use-state nil)
        invalid? (or (string/blank? (:title value))
                     (string/blank? (:task-title value))
                     (empty? (:check-in-days value)))
        update! (fn [key next-value]
                  (set-value! #(assoc % key next-value)))
        toggle-weekday! (fn [day checked?]
                          (set-value! #(update % :check-in-days
                                               (if checked? conj disj) day)))
        submit! (fn [event]
                  (.preventDefault event)
                  (if invalid?
                    (do
                      (set-tab! (if (string/blank? (:title value)) :goal :check-ins))
                      (set-error! (t :goal.form/required-error)))
                    (do
                      (set-saving! true)
                      (set-error! nil)
                      (-> ((if goal
                             #(goals-handler/update-goal! goal %)
                             goals-handler/create-goal!)
                           (save-payload value))
                          (p/then (fn [_]
                                    (set-saving! false)
                                    (on-saved)))
                          (p/catch (fn [_]
                                     (set-saving! false)
                                     (set-error! (t :goal.form/save-error))))))))]
    [:form.goal-form {:on-submit submit!}
     [:div.goal-form-tabs {:role "tablist" :aria-label (t :goal.form/sections-label)}
      [:button {:type "button"
                :role "tab"
                :aria-selected (= tab :goal)
                :on-click #(set-tab! :goal)}
       (t :goal.form/goal-tab)]
      [:button {:type "button"
                :role "tab"
                :aria-selected (= tab :check-ins)
                :on-click #(set-tab! :check-ins)}
       (t :goal.form/check-ins-tab)]]
     (case tab
       :goal
       [:section.goal-form-section {:role "tabpanel"}
        [:label.goal-field
         [:span (t :goal.form/title-label)]
         (shui/input {:value (:title value)
                      :auto-focus true
                      :required true
                      :placeholder (t :goal.form/title-placeholder)
                      :on-change #(update! :title (input-value %))})]]

       :check-ins
       [:section.goal-form-section {:role "tabpanel"}
        [:label.goal-field
         [:span (t :goal.form/check-in-text-label)]
         (shui/input {:value (:task-title value)
                      :auto-focus true
                      :required true
                      :placeholder (t :goal.form/check-in-text-placeholder)
                      :on-change #(update! :task-title (input-value %))})]
        [:fieldset.goal-weekdays
         [:legend (t :goal.form/task-days-label)]
         [:div.goal-weekday-options
          (for [day weekdays]
            [:label.goal-weekday {:key day}
             (shui/checkbox
              {:checked (contains? (:check-in-days value) day)
               :on-checked-change #(toggle-weekday! day (boolean %))})
             [:span (weekday-label day)]])]]
        [:label.goal-field.goal-reminder
         [:span (t :goal.form/reminder-label)]
         (shui/input {:value (:reminder value)
                      :type "time"
                      :on-change #(update! :reminder (input-value %))})]])
     (when error
       [:p.goal-form-error {:role "alert"} error])
     [:div.goal-form-actions
      (shui/button {:type "button" :variant :ghost :disabled saving? :on-click on-cancel}
                   (t :ui/cancel))
      (shui/button {:type "submit" :disabled saving?}
                   (if saving?
                     (t :goal.form/saving)
                     (if goal (t :goal.form/save) (t :goal.form/create))))]]))

(defn- month-start [journal-day]
  (let [value (date-time-util/int->local-date journal-day)]
    (js/Date. (.getFullYear value) (.getMonth value) 1)))

(defn- shift-month [month amount]
  (js/Date. (.getFullYear month) (+ (.getMonth month) amount) 1))

(defn- month-days [month]
  (let [year (.getFullYear month)
        month-index (.getMonth month)
        count (.getDate (js/Date. year (inc month-index) 0))
        monday-offset (mod (+ (.getDay month) 6) 7)]
    (vec (concat (repeat monday-offset nil)
                 (map (fn [day]
                        (date-time-util/date->int (js/Date. year month-index day)))
                      (range 1 (inc count)))))))

(defn- calendar-state [goal summary journal-day today-day]
  (let [record (get (:record-by-day summary) journal-day)
        status (some-> record goals/record-status)]
    (cond
      (= status goals/done-status) :completed
      (= status goals/missed-status) :missed
      (and (< journal-day today-day)
           (goals/scheduled-on-day? goal journal-day)
           (goals/active-on-day? goal (:records summary) journal-day)) :missed
      :else nil)))

(hsx/defc goal-calendar
  [{:keys [goal summary today-day]}]
  (let [[month set-month!] (hooks/use-state (month-start today-day))
        days (month-days month)]
    [:section.goal-calendar {:aria-label (t :goal/calendar-label)}
     [:header.goal-calendar-header
      (shui/button {:variant :ghost :size :sm
                    :aria-label (t :goal/previous-month)
                    :on-click #(set-month! (shift-month month -1))}
                   (ui/icon "chevron-left" {:size 16}))
      [:h3 (i18n/locale-format-date month {:month "long" :year "numeric"})]
      (shui/button {:variant :ghost :size :sm
                    :aria-label (t :goal/next-month)
                    :on-click #(set-month! (shift-month month 1))}
                   (ui/icon "chevron-right" {:size 16}))]
     [:div.goal-calendar-weekdays {:aria-hidden true}
      (for [day (take 7 weekdays)]
        [:span {:key day} (weekday-label day)])]
     [:div.goal-calendar-grid
      (for [[index journal-day] (map-indexed vector days)]
        (if-not journal-day
          [:span.goal-calendar-blank {:key (str "blank-" index)}]
          (let [state (calendar-state goal summary journal-day today-day)
                scheduled? (goals/scheduled-on-day? goal journal-day)
                today? (= journal-day today-day)]
            [:div.goal-calendar-day
             {:key journal-day
              :data-state (some-> state name)
              :data-today today?
              :aria-label (str (i18n/locale-format-date
                                (date-time-util/int->local-date journal-day))
                               (case state
                                 :completed (str ", " (t :goal/completed))
                                 :missed (str ", " (t :goal/missed))
                                 ""))}
             [:span.goal-calendar-number
              (.getDate (date-time-util/int->local-date journal-day))]
             (case state
               :completed [:span.goal-calendar-mark (ui/icon "check" {:size 15})]
               :missed [:span.goal-calendar-mark (ui/icon "x" {:size 15})]
               (when scheduled? [:span.goal-calendar-scheduled "·"]))])))] ]))

(defn- schedule-label [goal]
  (->> weekdays
       (filter #(not (zero? (bit-and (goals/check-in-days-mask goal)
                                     (bit-shift-left 1 %)))))
       (map weekday-label)
       (string/join ", ")))

(defn- confirm-delete! [goal on-deleted]
  (-> (shui/dialog-confirm!
       {:title (t :goal/delete-confirm-title)
        :content [:p (:block/title goal)]
        :outside-cancel? true
        :cancel-label (t :ui/cancel)
        :ok-label (t :goal/delete)})
      (p/then (fn []
                (-> (goals-handler/delete-goal! goal)
                    (p/then on-deleted))))
      (p/catch #())))

(hsx/defc goal-detail
  [{:keys [goal records today-day on-close]}]
  (let [[editing? set-editing!] (hooks/use-state false)
        summary (goals/goal-summary goal records today-day)
        current (get-in summary [:streaks :current] 0)
        longest (get-in summary [:streaks :longest] 0)]
    [:section.goal-detail
     [:button.goal-back {:type "button" :on-click on-close}
      (ui/icon "arrow-left" {:size 16})
      (t :goal/all-goals)]
     (if editing?
       (goal-form {:goal goal
                   :on-saved #(set-editing! false)
                   :on-cancel #(set-editing! false)})
       [:<>
        [:header.goal-detail-header
         [:div
          [:h2 (:block/title goal)]
          [:p (:logseq.property.goal/daily-check-in goal)]]
         [:div.goal-detail-actions
          (shui/button {:variant :outline :size :sm :on-click #(set-editing! true)}
                       (t :goal/edit))
          (shui/button {:variant :ghost :size :sm
                        :class "goal-delete"
                        :on-click #(confirm-delete! goal on-close)}
                       (t :goal/delete))]]
        [:div.goal-streak-summary
         [:div [:strong current] [:span (t :goal/current-streak)]]
         [:div [:strong longest] [:span (t :goal/longest-streak)]]]
        [:p.goal-schedule (t :goal/schedule-label (schedule-label goal))]
        (goal-calendar {:goal goal :summary summary :today-day today-day})])]))

(hsx/defc goal-list-item
  [{:keys [goal records today-day on-open]}]
  (let [summary (goals/goal-summary goal records today-day)
        current (get-in summary [:streaks :current] 0)]
    [:button.goal-list-item {:type "button" :on-click #(on-open goal)}
     [:span.goal-list-copy
      [:strong (:block/title goal)]
      [:span (:logseq.property.goal/daily-check-in goal)]]
     [:span.goal-list-streak
      [:strong current]
      [:span (t :goal/day-streak)]]
     (ui/icon "chevron-right" {:size 17})]))

(hsx/defc goals-page
  []
  (let [data (db-hooks/use-resource [:goals])
        [creating? set-creating!] (hooks/use-state false)
        [selected-id set-selected-id!] (hooks/use-state nil)
        today-day (date/today-journal-day)
        all-goals (:goals data)
        records (:records data)
        running-goals (filterv #(not= :logseq.property.goal/state.archived
                                     (goals/entity-ident (:logseq.property.goal/state %)))
                               all-goals)
        selected-goal (some #(when (= selected-id (:db/id %)) %) running-goals)]
    (hooks/use-effect!
     (fn [] (goals-handler/ensure-check-ins!) nil)
     [])
    [:main.ls-goals
     [:header.goals-header
      [:h1 (t :goal/page-title)]
      (when (and (not creating?) (nil? selected-goal))
        (shui/button {:on-click #(set-creating! true)}
                     (ui/icon "plus" {:size 16})
                     (t :goal/create)))]
     (cond
       (nil? data)
       [:div.goals-loading {:aria-label (t :goal/loading)} [:span] [:span] [:span]]

       selected-goal
       (goal-detail {:goal selected-goal
                     :records records
                     :today-day today-day
                     :on-close #(set-selected-id! nil)})

       creating?
       [:section.goal-create-panel
        (goal-form {:on-saved #(set-creating! false)
                    :on-cancel #(set-creating! false)})]

       (empty? running-goals)
       [:section.goals-empty
        (ui/icon "target" {:size 28})
        [:h2 (t :goal/empty)]
        [:p (t :goal/empty-desc)]
        (shui/button {:variant :outline :on-click #(set-creating! true)}
                     (t :goal/create-first))]

       :else
       [:section.goals-list {:aria-label (t :goal/running-goals)}
        (for [goal running-goals]
          (goal-list-item {:key (str (:block/uuid goal))
                           :goal goal
                           :records records
                           :today-day today-day
                           :on-open #(set-selected-id! (:db/id %))}))])]))
