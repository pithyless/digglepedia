(ns digglepedia.web.ui.app
  (:require [digglepedia.logging :as log]
            [digglepedia.util :as u :refer [getx getx-in]]
            [sablono.core :as html :refer-macros [html]]
            [digglepedia.web.dom.box :refer [box h-box v-box gap flex-flow-style]]
            [clojure.string :as str]
            [om.next :as om :refer-macros [defui]]))


;;
;; ----- String Utils ------
;;

(defn slug->str [s]
  (-> s
      (str/split ":")
      (last)
      (str/replace "_" " ")))

(defn px [n]
  (str n "px"))

(defn percent [n]
  (str n "%"))

(defn em [n]
  (str n "em"))


;;
;; ----- Widgets ------
;;

(defn ui-icon
  "Renders a square image"
  [size path]
  (let [klass (-> path (str/split #"/") (last) (str/split #"\.png") (first))]
    (box :class (str "s s-" klass))))

(defn ui-text
  "Renders a text label"
  ([string] (box :child (str string)))
  ([klass string] (box :class klass :child (str string))))

(defn ui-span
  "Renders icon and label together"
  [children]
  (h-box :align :center
         :gap (em 0.25)
         :children children))

(defn ui-section [title children]
  (v-box :children
         (into [(ui-text "section" title)] children)))

(def ui-clock-icon
  (ui-icon (px 16) "buff_time.png"))



;;
;; ----- Effects ------
;;

(defn ui-stat-value [props]
  (let [amt         (getx props :amount)
        amt-icon    (getx-in props [:target :icon])
        boost       (get props :amount/boost)
        boost-icon  (get-in props [:amount/modifier :icon])
        amt-label   [(ui-icon (px 16) amt-icon)
                     (ui-text amt)]
        boost-label (when boost
                      [(ui-text "(")
                       (ui-text "+")
                       (ui-text boost)
                       (ui-text "×")
                       (ui-icon (px 16) boost-icon)
                       (ui-text ")")
                       ])]
    (if boost
      (ui-span (into amt-label boost-label))
      (ui-span amt-label))))


;; TODO - pluralize Taxa
(defn ui-effect-defaults [props]
  (let [affects-caster (when (get props :affects/caster)
                         (ui-text "Can affect caster"))
        affects-taxa (when-let [taxa (get-in props [:affects/taxa :slug])]
                       (ui-text (str "Only affects " (slug->str taxa))))]
    (filterv identity [affects-caster affects-taxa])))

(defn render-ui-effect [props]
  (let [parent-slug  (getx-in props [:parent :slug])
        amount-stat? (get props :amount)
        defaults     (ui-effect-defaults props)
        header       (ui-text "section" (slug->str parent-slug))
        amount-stats (when amount-stat? (ui-stat-value props))]
    (v-box :children
           (into [header amount-stats] defaults))))

(defn render-ui-effect-damages [all-props]
  (let [sorted-props (reverse (sort-by :amount all-props))
        props        (first all-props)
        parent-slug  (getx-in props [:parent :slug])
        defaults     (ui-effect-defaults props)
        header       (ui-text "section" (slug->str parent-slug))
        dmg-stats    (h-box :flex-wrap true ;; TODO - need to fix wrapping margin issues
                            :gap (em 0.75)
                            :children
                            (mapv ui-stat-value sorted-props))]
    (v-box :children
           (into [header dmg-stats] defaults))))


(defui Effect
  static om/IQuery
  (query [this]
    [:affects/caster
     {:affects/taxa [:slug]}
     :amount
     :amount/boost
     {:amount/modifier [:slug :icon]}
     {:parent [:slug]}
     {:target [:slug :icon {:parent [:slug]}]}])

  Object
  (render [this]
    (html
     (render-ui-effect (om/props this)))))

(def ui-effect (om/factory Effect))


(defui EffectDamages
  Object
  (render [this]
    (html
     (render-ui-effect-damages (om/props this)))))

(def ui-effect-damages (om/factory EffectDamages))


(def damage-stat-slugs
  #{"Effects:Damages" "Effects:Drains_Life"})

(defui EffectsGroup
  static om/IQuery
  (query [this]
    [{:effects (om/get-query Effect)}])

  Object
  (render [this]
    (let [all-props   (getx (om/props this) :effects)
          ;; TODO - replace with comp and transducer friendly group-by
          dmg-props   (->> all-props
                           (filterv #(contains? damage-stat-slugs
                                                (getx-in % [:parent :slug])))
                           (group-by (fn [e] [(get-in e [:parent :slug])
                                              (get-in e [:affects/taxa])]))
                           (mapv (fn [[k v]] (ui-effect-damages v))))
          other-props (filterv #(not (contains? damage-stat-slugs
                                                (getx-in % [:parent :slug]))) all-props)]
      (html
       (v-box :children
              (into dmg-props
                    (mapv ui-effect other-props)))))))

(def ui-effects-group (om/factory EffectsGroup))


;;
;; ----- Spell ------
;;

(defui Spell
  static om/IQuery
  (query [this]
    [:slug :description :icon :cooldown/turns
     {:effects (om/get-query Effect)}
     {:buffs [:max/turns
              {:effects (om/get-query Effect)}]}])

  Object
  (render [this]
    (let [props       (om/props this)
          title       (ui-text "title" (slug->str (get props :slug)))
          icon        (ui-icon (px 32) (get props :icon))
          header      (ui-span [icon title])
          description (ui-text (get props :description))
          cooldown    (when-let [turns (get props :cooldown/turns)]
                        (ui-section "Cooldown"
                                    [(ui-span [ui-clock-icon
                                               (ui-text (str turns " turns"))])]))
          effects     (when-let [effects (get props :effects)]
                        (ui-effects-group {:effects effects}))]
      (html
       (v-box :size "1 1 auto"
              :children
              [header description
               (h-box :children
                      [(box :size "50%" :child cooldown)
                       (box :size "50%" :child effects)])])))))

(def ui-spell (om/factory Spell))


;;
;; ----- Layout ------
;;

(defn ui-layout-loading [props]
  (ui-text (str "Loading... " (getx props :agg/slugs-count))))

(defn- ui-layout-header-detail []
  (h-box :height "100%"
         :align :center
         :justify :end
         :children
         [(box :class "logo" :child "Digglepedia")]))

(defn- ui-layout-list [props]
  (v-box :height "100%"
         :justify :around
         :children
         (map ui-spell (getx props :ui/filtered-slugs))))

(defn- ui-layout-detail [props]
  (box :size "100%"
       :child (ui-spell (getx props :ui/active-slug))))

(defn- ui-layout-filters [props]
  (v-box :children
         [(ui-text "section" "Filters")
          (ui-text (str "Total: " (getx props :agg/slugs-count)))]))

(defn ui-layout-dashboard [props]
  (h-box :height "100%"
         :width  "100%"
         :children
         [(v-box :size "20%"
                 :class "layout-left"
                 :children
                 [(box :size "50px" :class "layout-left-head" :child "")
                  (box :size "100%" :class "layout-left-body" :child (ui-layout-filters props))])
          (v-box :size "40%"
                 :class "layout-mid"
                 :children
                 [(box :size "50px" :class "layout-mid-head" :child "")
                  (box :size "100%" :class "layout-mid-body" :child (ui-layout-list props))])
          (v-box :size "40%"
                 :class "layout-right"
                 :children
                 [(box :size "50px" :class "layout-right-head" :child (ui-layout-header-detail))
                  (box :size "100%" :class "layout-right-body" :child (ui-layout-detail props))])]))


;;
;; ----- App ------
;;

(defui App
  static om/IQuery
  (query [this]
    [:ui/loading
     :agg/slugs-count
     {:ui/filtered-slugs (om/get-query Spell)}
     {:ui/active-slug (om/get-query Spell)}])

  Object
  (render [this]
    (let [{:keys [:ui/loading] :as props} (om/props this)]
      (if loading
        (html (ui-layout-loading props))
        (html (ui-layout-dashboard props))))))
