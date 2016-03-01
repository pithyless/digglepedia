(ns digglepedia.game.xml
  (:require [digglepedia.logging :as log]
            [digglepedia.util :as u :refer [getx getx-in parse-int parse-float round compact-map]]
            [clojure.string :as str]
            [tubax.core :refer [xml->clj]]
            [com.rpl.specter :as s]))


;;
;; ----- Utils -----
;;


(defn tag= [tag] (s/filterer #(= (:tag %) tag)))

(defn merge-root [m m2]
  (merge-with into m (compact-map m2)))

(defn merge-compact [m m2]
  (merge m (compact-map m2)))

(defn merge-compact-maps [maps]
  (let [merge2 (fn [a b] (if (coll? a) (into a b) b))]
    (reduce
      (fn [a b] (merge-with merge2 a (compact-map b)))
      {} maps)))

(defn step-to-percentage [v]
  (round (* 100 (/ 1.0 (parse-float v)))))

(defn greater-than-0 [v]
  (when (> v 0) v))

(defn format-slug [slug]
  (str/replace slug " " "_"))

(defn lower-case-keys [m]
  (zipmap (map u/lower-case-keyword (keys m)) (vals m)))

(defn sanitize-attributes
  ([attributes]
   (sanitize-attributes attributes []))
  ([attributes ignore-attributes]
   (as-> attributes attrs
         (lower-case-keys attrs)
         (apply dissoc attrs ignore-attributes))))

(defn upsert-slug
  "A reference to slug via :find-or-create,
  i.e. {:slug name} not [:slug name]
  This gives fewer guarantees (we can accidentally create invalid slugs),
  but allows to link to slugs not yet visited."
  [slug] {:slug (format-slug slug)})

(defn true-if-1 [value]
  (if (= "1" value) true nil))


;;
;; ----- Parse -----
;;


(defmulti parse-tag
  (fn [node] (-> (getx node :tag) (u/lower-case-keyword))))
(defmulti parse-effect
  (fn [node] (keyword (str/lower-case (getx-in node [:attributes :type])))))

(defn parse-xml [xml]
  (->> xml
       (xml->clj)
       (parse-tag)))


;;
;; ----- Ignore -----
;;


(defmethod parse-tag :fountain [node] {})
(defmethod parse-tag :power [node] {})   ;; TODO: weapon power?
(defmethod parse-tag :anim [xml] {})
(defmethod parse-tag :impact [xml] {})
(defmethod parse-tag :ai [xml] {})
(defmethod parse-tag :halo [xml] {})


;;
;; ----- Helpers -----
;;

(defn taxa-slug [taxa]
  [:slug (str "Taxa:" (str/capitalize taxa))])

(defn damage-type-slug [dmg]
  [:slug (str (str/capitalize (name dmg)) "_Damage")])

(def weapon-types
  {"0" "Items:Weapons:Swords"
   "1" "Items:Weapons:Axes"
   "2" "Items:Weapons:Maces"
   "3" "Items:Weapons:Staffs"
   "4" "Items:Weapons:Bows"
   "5" "Items:Weapons:Thrown"
   "6" "Items:Weapons:Bolts"
   "7" "Items:Weapons:Daggers"
   "8" "Items:Weapons:Polearms"})

;;
;; ----- Text ------
;;


(defmethod parse-tag :text [node] {})


;;
;; ----- Effects -----
;;

;; NOOPs
(defmethod parse-effect :buff [node] {})
(defmethod parse-effect :sacrificeartifact [node] {})

;; TODOs
(defmethod parse-effect :charm [node] {})
(defmethod parse-effect :confuse [node] {})
(defmethod parse-effect :consumebooze [node] {})
(defmethod parse-effect :consumefood [node] {})
(defmethod parse-effect :corrupt [node] {})
(defmethod parse-effect :create [node] {})
(defmethod parse-effect :dig [node] {})
(defmethod parse-effect :draincastermana [node] {})
(defmethod parse-effect :drainmana [node] {})
(defmethod parse-effect :dot [node] {})
(defmethod parse-effect :fear [node] {})
(defmethod parse-effect :heal [node] {})
(defmethod parse-effect :knock [node] {})
(defmethod parse-effect :lockdown [node] {})
(defmethod parse-effect :oil [node] {})
(defmethod parse-effect :pacify [node] {})
(defmethod parse-effect :randomizeartifact [node] {})
(defmethod parse-effect :randomizewand [node] {})
(defmethod parse-effect :rechargeanvil [node] {})
(defmethod parse-effect :rechargewand [node] {})
(defmethod parse-effect :removebuffbyname [node] {})
(defmethod parse-effect :removeinvisibility [node] {})
(defmethod parse-effect :resurrection [node] {})
(defmethod parse-effect :rift [node] {})
(defmethod parse-effect :spawn [node] {})
(defmethod parse-effect :spawnitemfromlist [node] {})
(defmethod parse-effect :spellpoints [node] {})
(defmethod parse-effect :stealitem [node] {})
(defmethod parse-effect :summon [node] {})
(defmethod parse-effect :swapwithmonster [node] {})
(defmethod parse-effect :targetblink [node] {})
(defmethod parse-effect :teleport [node] {})
(defmethod parse-effect :triggerfromlist [node] {})
(defmethod parse-effect :uncurse [node] {})
(defmethod parse-effect :vendingmachinesteal [node] {})


(def damage-types
  #{:acidic :aethereal :asphyxiative :blasting :conflagratory
    :crushing :existential :hyperborean :necromantic :piercing
    :putrefying :righteous :slashing :toxic :transmutative :voltaic})

(def damage-types-with-modifiers
  (mapv #(vector % (keyword (str (name %) "f"))) damage-types))

(def damage-effect-extra-attrs
  [:bleed :burn :midas])

(def damage-effect-non-option-attrs
  (concat (flatten damage-types-with-modifiers)
          damage-effect-extra-attrs
          [:type :sfx :primarybuff :primaryscale :secondarybuff :secondaryscale]))

(defn amount-modifier-ref [attrs default]
  (condp #(contains? %2 %1) attrs
    :primarybuff    [:primary/buff    (-> attrs (:primarybuff)    (parse-int))]
    :primaryscale   [:primary/scale   (-> attrs (:primaryscale)   (parse-int))]
    :secondarybuff  [:secondary/buff  (-> attrs (:secondarybuff)  (parse-int))]
    :secondaryscale [:secondary/scale (-> attrs (:secondaryscale) (parse-int))]
    default))

(defn parse-effect-damage-et-al
  [{:keys [:attributes]} parent-name]
  (let [attrs   (sanitize-attributes attributes)
        damages (for [[d df] damage-types-with-modifiers]
                  (let [[dv dfv] ((juxt d df) attrs)]
                    (merge-compact-maps
                     [(when (or dv dfv) {:parent [:slug parent-name]
                                         :target (damage-type-slug d)
                                         :amount 0})
                      (when dfv {:amount/modifier
                                 (amount-modifier-ref attrs [:slug "Magic_Power"])
                                 :amount/boost (parse-float dfv)})
                      (when dv  {:amount (parse-int dv)})])))
        extras  (for [[k v] (select-keys attrs damage-effect-extra-attrs)]
                  (case k
                    :bleed {:parent [:slug "Effects:Starts_Bleeding"]}
                    :burn  {:parent [:slug "Effects:Burns_Target"]}
                    :midas {:parent [:slug "Effects:Midas"]}))
        options (merge-compact-maps
                 (for [[k v] (apply dissoc attrs damage-effect-non-option-attrs)]
                   (case k
                     :affectscaster {:affects/caster (true-if-1 v)}
                     :self          {:affects/self (true-if-1 v)}
                     :percent       {:chance/percentage (parse-int v)}
                     :taxa          {:affects/taxa (taxa-slug v)})))
        results (->> (concat extras damages)
                     (remove empty?)
                     (mapv #(merge options %)))]
    [{:effects results}]))

(defmethod parse-effect :damage [node]
  (parse-effect-damage-et-al node "Effects:Damages"))

(defmethod parse-effect :drain [node]
  (parse-effect-damage-et-al node "Effects:Drains_Life"))

(defmethod parse-effect :bleed [node]
  [{:effects [{:parent [:slug "Effects:Starts_Bleeding"]}]}])

(defmethod parse-effect :blink
  [{:keys [:attributes]}]
  (as-> attributes result
    (for [[k v] (sanitize-attributes result)]
         (case k
           :type {:parent [:slug "Effects:Randomly_Teleports"]}
           :self {:affects/self (true-if-1 v)}))
    (merge-compact-maps result)
    (vector {:effects [result]})))

(defmethod parse-effect :sleep
  [{:keys [:attributes]}]
  (as-> attributes result
    (for [[k v] (sanitize-attributes result)]
         (case k
           :type   {:parent [:slug "Effects:Puts_to_Sleep"]}
           :amount {:max/turns (parse-int v)}))
    (merge-compact-maps result)
    (vector {:effects [result]})))

(defmethod parse-effect :paralyze
  [{:keys [:attributes]}]
  (as-> attributes result
    (for [[k v] (sanitize-attributes result)]
         (case k
           :type          {:parent [:slug "Effects:Paralyzes"]}
           :turns         {:max/turns (parse-int v)}
           :affectscaster {:affects/caster (true-if-1 v)}
           :self          {:affects/self (true-if-1 v)}
           :after         {:after/turns (parse-int v)}
           :amount        {:chance/percentage (step-to-percentage v)}
           :percentage    {:chance/percentage (parse-int v)}
           :resistable    {:irresistable (when (= v "0") true)}))
    (merge-compact-maps result)
    (vector {:effects [result]})))

(def noop-trigger-attributes
  [:requirebuff])

(defmethod parse-effect :trigger
  [{:keys [:attributes]}]
  (as-> attributes result
    (for [[k v] (sanitize-attributes result noop-trigger-attributes)]
         (case k
           :type           {:parent [:slug "Triggers:Effect"]}
           :spell          {:target (upsert-slug v)}
           :amount         {:after/turns (greater-than-0 (parse-int v))}
           :affectscorpses {:affects/corpses (true-if-1 v)}
           :affectscaster  {:affects/caster (true-if-1 v)}
           :percent        {:chance/percentage (parse-int v)}
           :self           {:affects/self (true-if-1 v)}
           :taxa           {:affects/taxa (taxa-slug v)}))
    (merge-compact-maps result)
    (vector {:triggers [result]})))

(defmethod parse-tag :effect [node]
  (parse-effect node))


;;
;; ----- Requirements -----
;;


(def todo-requirements-attributes
  ;; TODO
  [:level :booze])

(defn parse-requirements-attributes [attributes]
  (merge-compact-maps
   (for [[k v] (sanitize-attributes attributes todo-requirements-attributes)]
         (case k
           :mincost    {:amount/min (parse-int v)}
           :mp         {:parent [:slug "Requires:Mana_Cost"]
                        :amount (parse-int v)}
           :savvybonus {:amount/modifier [:slug "Savvy"]
                        :amount/boost    (parse-float v)}
           :shield     {:parent [:slug "Requires:Requires_Shield"]}
           :weapon     {:parent [:slug "Requires:Requires_Weapon"]
                        :target [:slug (getx weapon-types v)]}))))

(defmethod parse-tag :requirements
  [{:keys [:attributes]}]
  (let [prereq (parse-requirements-attributes attributes)]
    (if (empty? prereq) [] [{:prereqs [prereq]}])))


;;
;; ----- Buffs -----
;;


(defmethod parse-tag :thrownbuff [node] {})
(defmethod parse-tag :playerhiteffectbuff [node] {})
(defmethod parse-tag :sightbuff [node] {})
(defmethod parse-tag :sensewallsflag [node] {})
(defmethod parse-tag :invisible [node] {})
(defmethod parse-tag :mute [node] {})
(defmethod parse-tag :polymorph [node] {})


;; Primary + Secondary

(defn parse-primary-and-secondary-buff [attr {:keys [:attributes]}]
  [{:effects
    [(merge-compact-maps
      (concat
       [{:parent [:slug "Effects:Buffs"]}]
       (for [[k v] (sanitize-attributes attributes)]
         (case k
           :id     {:target [attr (parse-int v)]}
           :amount {:amount (parse-int v)}))))]}])

(defmethod parse-tag :primarybuff [node]
  (parse-primary-and-secondary-buff :primary/buff node))

(defmethod parse-tag :secondarybuff [node]
  (parse-primary-and-secondary-buff :secondary/buff node))


;; Damage + Resist

(defn parse-damage-and-resist-buff
  [{:keys [:attributes]} slug-name-fn]
  [{:effects
    (let [attrs (sanitize-attributes attributes)]
      (assert (clojure.set/subset? (set (keys attrs)) damage-types))
      (for [[k v] attrs]
        {:parent [:slug "Effects:Buffs"]
         :target [:slug (slug-name-fn k)]
         :amount (parse-int v)}))}])

(defmethod parse-tag :resistbuff [node]
  (parse-damage-and-resist-buff
    node #(str (str/capitalize (name %)) "_Resistance")))

(defmethod parse-tag :damagebuff [node]
  (parse-damage-and-resist-buff
    node #(str (str/capitalize (name %)) "_Damage")))


;; Target_Hit + Player_Hit

(defmethod parse-tag :targethiteffectbuff
  [{:keys [:attributes]}]
  (as-> attributes result
    (for [[k v] (sanitize-attributes result)]
         (case k
           :name       {:target (upsert-slug v)}
           :taxa       {:affects/taxa (taxa-slug v)}
           :after      {:after/turns (parse-int v)}
           :percentage {:chance/percentage (parse-int v)}))
    (merge-compact-maps result)
    (merge {:parent [:slug "Triggers:Target_Hit"]} result)
    (vector {:triggers [result]})))


;; Others

(defmethod parse-tag :dodgebuff
  [{:keys [:attributes]}]
  (as-> attributes result
    (for [[k v] (sanitize-attributes result)]
         (case k
           :name       {:target [:slug (format-slug v)]}
           :percentage {:chance/percentage (parse-int v)}))
    (merge-compact-maps result)
    (merge {:parent [:slug "Triggers:Effect"]} result)
    (vector {:triggers [result]})))

;; Buff

(def noop-buff-attributes
  [:usetimer :self :icon :smallicon :bad])

(defn parse-buff-attributes [attributes]
  (merge-compact-maps
    (concat
     [{:max/stacks -1}]
     (for [[k v] (sanitize-attributes attributes noop-buff-attributes)]
        (case k
          :stacksize      {:max/stacks (parse-int v)}
          :stackable      {:max/stacks (if (= "0" v) 1 nil)}
          :allowstacking  {:max/stacks (if (= "0" v) 1 nil)}
          :removable      {:buff/removable (true-if-1 v)}
          :manaupkeep     {:mana/upkeep (parse-int v)}
          :time           {:max/turns (parse-int v)}
          :brittle        {:max/hits (parse-int v)}
          :attacks        {:max/attacks (parse-int v)}
          :requiresshield {:prereqs [{:parent [:slug "Requires:Requires_Shield"]}]})))))

(defmethod parse-tag :buff
  [{:keys [:attributes :content]}]
  (let [root (parse-buff-attributes attributes)
        buff (merge-compact-maps
              (concat [root] (mapcat parse-tag content)))]
    (if (empty? buff) [] [{:buffs [buff]}])))


;;
;; ----- Spells -----
;;


(defmethod parse-tag :spelldb
  [{:keys [:content]}]
  (mapv parse-tag content))

(def spell-types
  {"adjacent"             "Spells:Adjacent"
   "beam"                 "Spells:Beam"
   "diggingbeam"          "Spells:Digging_Beam"
   "fireball"             "Spells:Fireball"
   "item"                 "Spells:Item"
   "knightlyleap"         "Spells:Knightly_Leap"
   "missile"              "Spells:Missile"
   "rook"                 "Spells:Rook"
   "self"                 "Spells:Self"
   "target"               "Spells:Target"
   "targetadjacentcorpse" "Spells:Target_Adjacent_Corpse"
   "targetcorpse"         "Spells:Target_Corpse"
   "targetemptyfloor"     "Spells:Target_Empty_Floor"
   "targetfloor"          "Spells:Target_Floor"
   "targetmonster"        "Spells:Target_Monster"
   "template"             "Spells:Template"})

(def mine-spell-attributes
  [:mine :mineradius :minetimer :minepermanent])

(defn parse-spell-mine [attributes]
  (as-> attributes result
        (sanitize-attributes result)
        (select-keys result mine-spell-attributes)
        (for [[k v] result]
             (case k
               :mine          {:parent [:slug "Effects:Mine"]}
               :mineradius    {:mine/radius (parse-int v)}
               :minetimer     {:max/turns (parse-int v)}
               :minepermanent {:mine/permanent (true-if-1 v)}))
        (merge-compact-maps result)
        (if (empty? result) [] (vector {:effects [result]}))))

(def noop-spell-attributes
  [:wand :minesmustbeunobstructed :mineuseglints :mineglintdensity :minespritedraworder
   :minesprite :minespritepngrate :minespritepngfirst :minespritepngnum :minespritepngseries])

;; TODO
(def todo-spell-attributes
  [:anchored :templateid :radius])

(defn parse-spell-attributes [attributes]
  (let [attrs (sanitize-attributes attributes
                                   (concat noop-spell-attributes mine-spell-attributes todo-spell-attributes))]
    (merge-compact-maps
      (concat
       (for [[k v] attrs]
             (case k
               :consumeitemtype nil
               :name            {:slug (format-slug v)}
               :type            {:parent [:slug (getx spell-types v)]}
               :icon            {:icon v}
               :downtime        {:cooldown/turns (parse-int v)}
               :self            {:affects/self (true-if-1 v)}
               :attack          {:effects [{:parent [:slug "Effects:Performs_Melee_Attack"]}]}
               :consumeitem     (when (= v "1")
                                  {:effects
                                   (merge-compact-maps
                                    [{:parent [:slug "Effects:Consumes_Item"]}
                                     {:target (when-let [kind (:consumeitemtype attrs)]
                                                (case kind
                                                  "mushroom" [:slug "Items:Mushrooms"]
                                                  "gem"      [:slug "Items:Gems"]
                                                  "potions"  [:slug "Items:Potions"]
                                                  "wands"    [:slug "Items:Wands"]
                                                  "artifact" [:slug "Items:Artifacts"]))}])})))
      (parse-spell-mine attributes)))))

(defmethod parse-tag :spell
  [{:keys [:attributes :content]}]
  (let [root (parse-spell-attributes attributes)]
    (merge-compact-maps
      (concat
        [root]
        (mapcat parse-tag content)))))


;;
;; ----- Skills ------
;;


(defmethod parse-tag :skilldb [node] {})


;;
;; ----- Items ------
;;


(defmethod parse-tag :itemdb
  [{:keys [:content]}]
  (mapv parse-tag content))

(defmethod parse-tag :food [node] {})
(defmethod parse-tag :trap [node] {})
(defmethod parse-tag :wand [node] {})
(defmethod parse-tag :mushroom [node] {})
(defmethod parse-tag :potion [node] {})
(defmethod parse-tag :gem [node] {})
(defmethod parse-tag :toolkit [node] {})
(defmethod parse-tag :casts [node] {})

(defmethod parse-tag :artifact
  [{:keys [:attributes]}]
  [{:artifact/quality (parse-int (getx attributes :quality))}])

(defmethod parse-tag :description
  [{:keys [:attributes]}]
  (let [attrs (sanitize-attributes attributes)]
    [{:description (or (:text attrs) (:monstertext attrs))}]))

(defmethod parse-tag :price
  [{:keys [:attributes]}]
  [{:item/price (parse-int (getx attributes :amount))}])

(defmethod parse-tag :weapon
  [node] {})
  ; [{:keys [:attributes :content]}]
  ; [{:parent [:slug (getx weapon-types (:type attributes))]
  ;   :level  (parse-int (getx attributes :level))
  ;   }])

(def armour-types
  {"chest"  "Items:Armours:Chest"
   "feet"   "Items:Armours:Feet"
   "hands"  "Items:Armours:Hands"
   "head"   "Items:Armours:Head"
   "legs"   "Items:Armours:Legs"
   "neck"   "Items:Armours:Neck"
   "ring"   "Items:Armours:Ring"
   "shield" "Items:Armours:Shield"
   "sleeve" "Items:Armours:Sleeve"
   "waist"  "Items:Armours:Waist"})

(defmethod parse-tag :armour
  [{:keys [:attributes :content]}]
  [{:parent [:slug (getx armour-types (:type attributes))]
    :level  (parse-int (getx attributes :level))
    }])

(defmethod parse-tag :item
  [{:keys [:attributes :content]}]
  (->> (concat
        [{:slug   (format-slug (:name attributes))
          :icon   (first (remove str/blank? ((juxt :iconFile) attributes)))
          :level  (some-> (:level attributes) (parse-int))
          :parent (some->> (:type attributes) (getx weapon-types) (vector :slug))}]
         (mapcat parse-tag content))
       (reduce merge-root {})))


;;
;; ----- Monsters ------
;;


(defmethod parse-tag :mondb [node] {})
