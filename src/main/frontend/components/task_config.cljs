(ns frontend.components.task-config
  (:require [frontend.context.i18n :as i18n :refer [t]]
            [frontend.handler.db-based.property :as db-property-handler]
            [frontend.handler.editor :as editor-handler]
            [frontend.handler.task-reminder :as task-reminder-handler]
            [frontend.ui :as ui]
            [frontend.util :as util]
            [frontend.util.task-reminder :as task-reminder]
            [io.factorhouse.hsx.core :as hsx]
            [logseq.shui.ui :as shui]
            [promesa.core :as p]))

(def ^:private priorities
  [{:ident :logseq.property/priority.high
    :value "High"
    :label-key :property.priority/high
    :color-class "bg-red-rx-06"}
   {:ident :logseq.property/priority.medium
    :value "Medium"
    :label-key :property.priority/medium
    :color-class "bg-yellow-rx-07"}
   {:ident :logseq.property/priority.low
    :value "Low"
    :label-key :property.priority/low
    :color-class "bg-blue-rx-07"}])

(defn- close-popup!
  []
  (shui/popup-hide!))

(defn- set-priority!
  [block priority-ident priority-value]
  (editor-handler/save-current-block!)
  (let [current-ident (:db/ident (:logseq.property/priority block))]
    (-> (if (= current-ident priority-ident)
          (db-property-handler/set-block-property!
           (:block/uuid block)
           :logseq.property/priority
           :logseq.property/empty-placeholder)
          (db-property-handler/batch-set-property-closed-value!
           [(:block/uuid block)]
           :logseq.property/priority
           priority-value))
        (p/finally close-popup!))))

(defn- set-reminder!
  [block scheduled-at]
  (editor-handler/save-current-block!)
  (-> (p/let [_ (db-property-handler/set-block-property!
                 (:block/uuid block)
                 :logseq.property/scheduled
                 scheduled-at)]
        (task-reminder-handler/schedule! block scheduled-at))
      (p/finally close-popup!)))

(defn- clear-reminder!
  [block]
  (-> (p/let [_ (db-property-handler/remove-block-property!
                 (:block/uuid block)
                 :logseq.property/scheduled)]
        (task-reminder-handler/cancel! block))
      (p/finally close-popup!)))

(defn- menu-heading
  [label]
  [:div.px-2.pb-1.pt-1.text-xs.font-medium.text-muted-foreground.select-none
   label])

(defn- selected-mark
  [selected?]
  [:span.ml-auto.flex.w-4.justify-end
   (when selected? (ui/icon "check" {:size 14}))])

(defn- priority-item
  [block {:keys [ident value label-key color-class]}]
  (let [selected? (= ident (:db/ident (:logseq.property/priority block)))]
    (shui/dropdown-menu-item
     {:key (name ident)
      :on-click #(set-priority! block ident value)}
     [:span.flex.w-full.items-center.gap-2
      [:span.rounded-full.shrink-0
       {:class (str "h-2.5 w-2.5 " color-class)
        :aria-hidden true}]
      [:span (t label-key)]
      (selected-mark selected?)])))

(defn- reminder-label
  [preset-id scheduled-at]
  (case preset-id
    :in-one-hour (t :block.task-config/in-one-hour)
    :in-three-hours (t :block.task-config/in-three-hours)
    :today-at-22 (t :block.task-config/today-at
                    (i18n/locale-format-time (js/Date. scheduled-at)))
    :tomorrow-at-10 (t :block.task-config/tomorrow-at
                       (i18n/locale-format-time (js/Date. scheduled-at)))
    :saturday-at-10 (t :block.task-config/saturday-at
                       (i18n/locale-format-time (js/Date. scheduled-at)))
    :monday-at-10 (t :block.task-config/monday-at
                     (i18n/locale-format-time (js/Date. scheduled-at)))))

(defn- reminder-item
  [block preset-id scheduled-at]
  (shui/dropdown-menu-item
   {:key (name preset-id)
    :disabled (nil? scheduled-at)
    :on-click #(when scheduled-at (set-reminder! block scheduled-at))}
   [:span.flex.w-full.items-center.gap-2
    (ui/icon "clock" {:size 15 :class "text-muted-foreground"})
    [:span (if scheduled-at
             (reminder-label preset-id scheduled-at)
             (t :block.task-config/today-at-passed))]]))

(defn- current-reminder-label
  [scheduled-at]
  (let [date (js/Date. scheduled-at)]
    (t :block.task-config/scheduled-for
       (str (i18n/locale-format-date date)
            " "
            (i18n/locale-format-time date)))))

(defn- task-config-menu
  [block]
  (let [times (task-reminder/preset-times (js/Date.))
        scheduled-at (:logseq.property/scheduled block)]
    [:div.task-config-menu.w-64.py-1
     (menu-heading (t :property.built-in/priority))
     (for [priority priorities]
       (priority-item block priority))
     (shui/dropdown-menu-separator)
     (menu-heading (t :block.task-config/remind-me))
     (for [preset-id [:in-one-hour :in-three-hours :today-at-22
                      :tomorrow-at-10 :saturday-at-10 :monday-at-10]]
       (reminder-item block preset-id (get times preset-id)))
     (when (number? scheduled-at)
       [:<>
        (shui/dropdown-menu-separator)
        [:div.px-2.py-1.text-xs.text-muted-foreground
         (current-reminder-label scheduled-at)]
        (shui/dropdown-menu-item
         {:on-click #(clear-reminder! block)}
         [:span.flex.items-center.gap-2
          (ui/icon "bell-off" {:size 15})
          (t :block.task-config/clear-reminder)])])]))

(hsx/defc task-config-button
  [block]
  (shui/button
   {:variant :ghost
    :size :sm
    :class "task-config-trigger h-6 w-6 min-w-0 p-0 text-muted-foreground hover:text-foreground"
    :title (t :block.task-config/tooltip)
    :aria-label (t :block.task-config/tooltip)
    :on-click (fn [e]
                (util/stop e)
                (shui/popup-show! e
                                  #(task-config-menu block)
                                  {:align :end
                                   :auto-focus? true
                                   :content-props {:class "w-64"}}))}
   (ui/icon "dots" {:size 16})))
