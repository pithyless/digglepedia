(ns digglepedia.cards.items
  (:require [digglepedia.logging :as log]
            [digglepedia.db :as db]
            [digglepedia.cards.state :refer [reconciler]]
            [digglepedia.cards.util :refer [expand-item make-slug-card]]
            [clojure.string :as str]
            [clojure.data]
            [sablono.core :as html :refer-macros [html]]
            [om.next :as om]
            [cljs.core.async :as a :refer [chan close! >! <!]]
            [cljs.test :refer-macros [is testing]]
            [devcards.core :as dc :refer-macros [defcard deftest]]
            [devcards-om-next.core :as don :refer-macros [om-next-root defcard-om-next]]))


;;
;; ----- Items ------
;;

(defcard "# Items")

;;
;; ----- Weapons ------
;;

(defcard "## Weapons")

(defcard (make-slug-card "Iron_Sword"))

(deftest test-iron-sword
  (testing
      (let [ds   (db/db reconciler)
            item (db/find-slug ds "Iron_Sword")]
      (is (= (expand-item item)
             {:description "A sturdy example of iron swordwork."
              :icon        "items/sword_craft_iron2.png"
              :game/mod    (db/find-slug ds "Mods:Base")
              :parent      (db/find-slug ds "Items:Weapons:Swords")
              :slug        "Iron_Sword"
              :item/price  1000
              :level       5})))))

;; TODO: damage-types, used-to-craft, can-be-crafted
;; TODO: (defcard (make-slug-card "Dwarven_Wifflemaul"))

;;
;; ----- Armours ------
;;

(defcard "## Armours")

(defcard (make-slug-card "Steel_Cuirass"))

(deftest test-steel-cuirass
  (testing
      (let [ds   (db/db reconciler)
            item (db/find-slug ds "Steel_Cuirass")]
      (is (= (expand-item item)
             {:description "Crafted of thick steel, this cuirass will serve you well.."
              :icon        "items/armour_craft_steel1.png"
              :game/mod    (db/find-slug ds "Mods:Base")
              :parent      (db/find-slug ds "Items:Armours:Chest")
              :slug        "Steel_Cuirass"
              :item/price  1640
              :level       4
              :effects     [{:parent (db/find-slug ds "Effects:Buffs")
                             :target (db/find-slug ds "Block_Chance")
                             :amount 6}
                            {:parent (db/find-slug ds "Effects:Buffs")
                             :target (db/find-slug ds "Armour_Absorption")
                             :amount 4}
                            {:parent (db/find-slug ds "Effects:Buffs")
                             :target (db/find-slug ds "Nimbleness")
                             :amount -2}
                            {:parent (db/find-slug ds "Effects:Buffs")
                             :target (db/find-slug ds "Magic_Power")
                             :amount -3}
                            {:parent (db/find-slug ds "Effects:Buffs")
                             :target (db/find-slug ds "Mana_Regeneration_Bonus")
                             :amount -1}
                            {:parent (db/find-slug ds "Effects:Buffs")
                             :target (db/find-slug ds "Piercing_Resistance")
                             :amount 2}]})))))

;; TODO: used-to-craft, can-be-crafted
;; TODO: (defcard (make-slug-card "Imperial_Boilerplate"))
