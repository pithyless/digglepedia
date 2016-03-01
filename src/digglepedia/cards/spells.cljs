(ns digglepedia.cards.spells
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
;; ----- Spells ------
;;


(defcard "# Spells")

(defcard "## Properties")

(deftest test-spell-properties
  (testing "A spell may have certain properties."
    (let [ds (db/db reconciler)]
      (is (= (:slug (db/find-slug ds "Duck_And_Cover!"))
             "Duck_And_Cover!") "All spells have slugs.")
      (is (= (:description (db/find-slug ds "Duck_And_Cover!"))
             "Fight smarter, not better! Can increase your defense for a while after you get hit.")
          "A description")
      (is (= (:icon (db/find-slug ds "Duck_And_Cover!"))
             "skills/skill_shieldbearer_small.png")
          "A 32x32 icon")
      (is (= (:cooldown/turns (db/find-slug ds "Duck_And_Cover!")) 48)
          "Spell cooldown")
      (is (= (:affects/self (db/find-slug ds "Invisible_Geometries_Drain")) true)
          "Affects self")
      (is (= (first (:effects (expand-item (db/find-slug ds "It_Belongs_In_A_Museum"))))
             {:parent (db/find-slug ds "Effects:Consumes_Item")
              :target (db/find-slug ds "Items:Artifacts")})
          "Consumes an item"))))

(defcard "## Types")

(deftest test-spell-types
  (testing "A spell is one of several types."
    (let [ds (db/db reconciler)]
      (doall
       (for [[spell-name spell-type] [["Blackjack" "Spells:Adjacent"]
                                      ["Smoke_Ray" "Spells:Beam"]
                                      ["Digging_Ray" "Spells:Digging_Beam"]
                                      ["Oil_Slick2" "Spells:Fireball"]
                                      ["Wand_Hack" "Spells:Item"]
                                      ["Knightly_Leap" "Spells:Knightly_Leap"]
                                      ["Firebolt" "Spells:Missile"]
                                      ["Invisible_Geometries" "Spells:Rook"]
                                      ["Sparkly_Glamour" "Spells:Self"]
                                      ["Curse_Removal" "Spells:Target"]
                                      ["Corpse_Drain" "Spells:Target_Adjacent_Corpse"]
                                      ["Corpse_Reviver" "Spells:Target_Corpse"]
                                      ["Summon_Wyrmling" "Spells:Target_Empty_Floor"]
                                      ["Feather_Cloud" "Spells:Target_Floor"]
                                      ["Sneak_Attack" "Spells:Target_Monster"]
                                      ["Cogito_Ergo_Splat" "Spells:Template"]]]
         (let [spell (db/find-slug ds spell-name)
               kind  (db/find-slug ds spell-type)]
               (is (= (:slug (:parent spell)) (:slug  kind))
                  (str spell-type " - " spell-name))))))))

(defcard "## Prerequisites")

(deftest test-spell-prereqs
  (testing "A spell may have a prerequisite."
    (let [ds (db/db reconciler)]
      (is (= (:prereqs (expand-item (db/find-slug ds "Animate_Blade_Being")))
             [{:parent          (db/find-slug ds "Requires:Mana_Cost")
               :amount          7
               :amount/boost    0.2
               :amount/modifier (db/find-slug ds "Savvy")
               :amount/min      3}]) "Requires Mana")
      (is (= (:prereqs (expand-item (db/find-slug ds "Duck_And_Cover!")))
             [{:parent (db/find-slug ds "Requires:Requires_Shield")}]) "Requires Shield")
      (is (= (:prereqs (expand-item (db/find-slug ds "Liechtenauer's_Overhau")))
             [{:parent (db/find-slug ds "Requires:Requires_Weapon")
               :target (db/find-slug ds "Items:Weapons:Swords")}]) "Requires Weapon"))))

(defcard
  "## Effects")

(deftest test-spell-effect-drains-life
  (testing "Drains Life"
    (let [ds (db/db reconciler)]
      (is (= (take 2 (:effects (expand-item (db/find-slug ds "Vampirism_Attack"))))
             [{:parent          (db/find-slug ds "Effects:Drains_Life")
               :target          (db/find-slug ds "Necromantic_Damage")
               :amount          1
               :amount/boost    0.6
               :amount/modifier (db/find-slug ds "Health_Regeneration_Bonus")
               :affects/taxa    (db/find-slug ds "Taxa:Demon")}
              {:parent          (db/find-slug ds "Effects:Drains_Life")
               :target          (db/find-slug ds "Necromantic_Damage")
               :amount          1
               :amount/boost    0.4
               :amount/modifier (db/find-slug ds "Health_Regeneration_Bonus")
               :affects/taxa    (db/find-slug ds "Taxa:Other")}])
          "With modifiers and specific taxanomies")
        (is (= (take 2 (:effects (expand-item (db/find-slug ds "Blungecap_Vampirism_Attack"))))
               [{:amount       3
                 :parent       (db/find-slug ds "Effects:Drains_Life")
                 :target       (db/find-slug ds "Necromantic_Damage")
                 :affects/taxa (db/find-slug ds "Taxa:Animal")}
                {:amount          0,
                 :parent          (db/find-slug ds "Effects:Drains_Life")
                 :target          (db/find-slug ds "Piercing_Damage")
                 :affects/taxa    (db/find-slug ds "Taxa:Animal")
                 :amount/boost    0.25
                 :amount/modifier (db/find-slug ds "Magic_Power")}])
            "With modifiers, without base amount"))))

(deftest test-spell-effect-damages
  (testing "Damages"
    (let [ds (db/db reconciler)]
      (is (= (take 2 (:effects (expand-item (db/find-slug ds "Doom_Hit"))))
             [{:parent            (db/find-slug ds "Effects:Damages")
               :target            (db/find-slug ds "Necromantic_Damage")
               :affects/caster    true
               :chance/percentage 100
               :amount            1}
              {:parent            (db/find-slug ds "Effects:Damages")
               :target            (db/find-slug ds "Necromantic_Damage")
               :chance/percentage 50
               :affects/caster    true
               :amount            2}])
          "With certain chance of happening")
      (is (= (first (:effects (expand-item (db/find-slug ds "Necropain"))))
             {:parent           (db/find-slug ds "Effects:Damages")
              :target           (db/find-slug ds "Necromantic_Damage")
              :amount          1
              :amount/boost    0.1
              :amount/modifier (db/find-slug ds "Magic_Power")
              :affects/caster  true
              :affects/self    true})
          "With boost and affects self"))))

(deftest test-spell-effect-puts-to-sleep
  (testing "Puts to Sleep"
    (let [ds (db/db reconciler)]
      (is (= (:effects (expand-item (db/find-slug ds "Blackjack")))
             [{:parent (db/find-slug ds "Effects:Damages")
               :target (db/find-slug ds "Crushing_Damage")
               :amount 2}
              {:parent    (db/find-slug ds "Effects:Puts_to_Sleep")
               :max/turns 2}])))))

(deftest test-spell-effect-melee-atack
  (testing "Performs Melee Attack"
    (let [ds (db/db reconciler)]
      (is (= (first (:effects (expand-item (db/find-slug ds "Adventurer_Aikido"))))
             {:parent (db/find-slug ds "Effects:Performs_Melee_Attack")})))))

(deftest test-spell-starts-bleeding
  (testing "Bleeds"
    (let [ds (db/db reconciler)]
      (is (= (first (:effects (expand-item (db/find-slug ds "Acid_Burn"))))
             {:parent (db/find-slug ds "Effects:Starts_Bleeding")
              :affects/caster true})
          "Damage that also starts bleeding.")
      (is (= (last (:effects (expand-item (db/find-slug ds "Norwegian_Axenado"))))
             {:parent (db/find-slug ds "Effects:Starts_Bleeding")})
          "Bleeding caused by effect."))))

(deftest test-spell-burns-target
  (testing "Burns"
    (let [ds (db/db reconciler)]
      (is (= (first (:effects (expand-item (db/find-slug ds "Ignition_Bolt"))))
             {:parent (db/find-slug ds "Effects:Burns_Target")})
          "Damage that also burns target"))))

(deftest test-spell-midas-touch
  (testing "Midas"
    (let [ds (db/db reconciler)]
      (is (= (first (:effects (expand-item (db/find-slug ds "Midas"))))
             {:parent (db/find-slug ds "Effects:Midas") :affects/caster true})
          "Killing drops more gold"))))

(deftest test-spell-teleportations
  (testing "Various Teleportations"
    (let [ds (db/db reconciler)]
      (is (= (first (:effects (expand-item (db/find-slug ds "Froda's_Jump_Discontinuity"))))
             {:parent (db/find-slug ds "Effects:Randomly_Teleports")
              :affects/self true})
          "Randomly teleport self.")
      ;; TODO - RotDG
      ;; (is (= (first (:effects (expand-item (db/find-slug ds "Love_Will_Teleport_Us_Apart"))))
      ;;        {:parent (db/find-slug ds "Effects:Displaces")})
      ;;     "Randomly teleport self.")
      ;; TODO
      ;; (is (= (first (:effects (expand-item (db/find-slug ds "Move_in_Mysterious_Way"))))
      ;;        {:parent (db/find-slug ds "Effects:Teleports")})
      ;;     "Randomly teleport self.")
      ;; TODO
      ;; (is (= (first (:effects (expand-item (db/find-slug ds "Monster_Blink"))))
      ;;        {:parent (db/find-slug ds "Effects:Randomly_Teleports_Target")})
      ;;     "Randomly teleport target")
      ;; TODO
      ;; (is (= (first (:effects (expand-item (db/find-slug ds "Get_Away_From_It_All"))))
      ;;        {:parent (db/find-slug ds "Effects:Ascends_Dungeon_Level")})
      ;;     "Teleports to upper level.")
      )))


(deftest test-spell-effect-paralyzes
  (testing "Paralyzes"
    (let [ds (db/db reconciler)]
      (is (= (last (:effects (expand-item (db/find-slug ds "Adventurer_Aikido"))))
             {:parent    (db/find-slug ds "Effects:Paralyzes")
              :max/turns 1})
          "Paralyzes for certain number of turns")
      (is (= (last (:effects (expand-item (db/find-slug ds "Electrical_Strike"))))
             {:parent            (db/find-slug ds "Effects:Paralyzes")
              :max/turns         1
              :affects/caster    true
              :chance/percentage 33})
          "Chance of paralysis (via amount)")
      (is (= (last (:effects (expand-item (db/find-slug ds "Talhoffer's_Lunge_Strike"))))
             {:parent            (db/find-slug ds "Effects:Paralyzes")
              :max/turns         1
              :after/turns       1
              :chance/percentage 50})
          "Chance of paralysis (via percentage) and after certain number of turns.")
      (is (= (first (:effects (expand-item (db/find-slug ds "Council's_Blessing"))))
             {:parent         (db/find-slug ds "Effects:Paralyzes")
              :max/turns      2
              :affects/caster true
              :irresistable   true})
          "Not resistable.")
      ;; TODO - CotW
      ;; (is (= (last (:effects (expand-item (db/find-slug ds "Medium_Dash_Attack_Fail"))))
      ;;        {:parent (db/find-slug ds "Effects:Paralyzes")
      ;;         :chance/percentage 50
      ;;         :max/turns 2
      ;;         :affects/self true})
      ;;     "Affects self.")
      )))

(defcard "## Triggers")

(deftest test-spell-triggers-effect
  (testing "Triggers from effect"
    (let [ds (db/db reconciler)]
      (is (= (take 2 (:triggers (expand-item (db/find-slug ds "Mark_of_Chthon"))))
             [{:parent      (db/find-slug ds "Triggers:Effect")
               :target      (db/find-slug ds "Curse_of_Weakness")
               :after/turns 27}
              {:parent      (db/find-slug ds "Triggers:Effect")
               :target      (db/find-slug ds "Minor_Blood_Debt")
               :after/turns 33}])
          "Triggers spells")
      (is (= (last (:triggers (expand-item (db/find-slug ds "Fungling"))))
             {:parent          (db/find-slug ds "Triggers:Effect")
              :target          (db/find-slug ds "Fungal_Bloom_2")
              :affects/corpses true
              :after/turns     4})
          "Affects corpses")
      (is (= (first (:triggers (expand-item (db/find-slug ds "Tentacular_Infestation_Bolt"))))
             {:parent         (db/find-slug ds "Triggers:Effect")
              :target         (db/find-slug ds "Infested_With_Tentacles")
              :affects/caster true})
          "Can affect caster")
      (is (= (first (filter #(= (:after/turns %) 6)
                            (:triggers (expand-item (db/find-slug ds "This_Root_Shall_Suffer")))))
             {:parent            (db/find-slug ds "Triggers:Effect")
              :target            (db/find-slug ds "Dire_Empowerment")
              :chance/percentage 50
              :after/turns       6})
          "Chance of effect (via percent)")
      (is (= (first (:triggers (expand-item (db/find-slug ds "Invisible_Geometries"))))
             {:parent       (db/find-slug ds "Triggers:Effect")
              :target       (db/find-slug ds "Invisible_Geometries_Drain")
              :affects/self true})
          "Affects self")
      (is (= (first (:triggers (expand-item (db/find-slug ds "Spirit_Steal"))))
             {:parent       (db/find-slug ds "Triggers:Effect")
              :target       (db/find-slug ds "Stolen_Spirit")
              :affects/taxa (db/find-slug ds "Taxa:Animal")})
          "Applies to specific taxanomy"))))


(deftest test-spell-triggers-target-hit
  (testing "Triggers when hit in melee"
    (let [ds (db/db reconciler)]
      (is (= (get-in (expand-item (db/find-slug ds "Mark_of_Chthon")) [:buffs 0 :triggers])
             [{:parent            (db/find-slug ds "Triggers:Target_Hit")
               :target            (db/find-slug ds "Nightmare")
               :after/turns       1
               :chance/percentage 42}])))))

(defcard "## Mines")

(deftest test-spell-mines
  (testing "Spell can be triggered via mines."
    (let [ds (db/db reconciler)]
      (is (= (first (:effects (expand-item (db/find-slug ds "Animate_Blade_Being"))))
             {:parent      (db/find-slug ds "Effects:Mine")
              :mine/radius 1
              :max/turns   96})
          "Mines have a radius and turn effect duration.")
      (is (= (first (:effects (expand-item (db/find-slug ds "Miasmatic_Putrefaction"))))
             {:parent         (db/find-slug ds "Effects:Mine")
              :mine/radius    2
              :max/turns      16
              :mine/permanent true})
          "Mines can be permanent."))))

(defcard "## Buffs")

(deftest test-spell-buffs
  (testing "Buffs are inner-entities with effects, triggers, etc."
    (let [ds (db/db reconciler)]
      (is (= (get-in (expand-item (db/find-slug ds "Transdimensional_Dodging")) [:buffs 0 :effects])
             [{:parent (db/find-slug ds "Effects:Buffs")
               :target (db/find-slug ds "Dodge_Chance")
               :amount 3}])
          "Buff effects")
      (is (= (get-in (expand-item (db/find-slug ds "Transdimensional_Dodging")) [:buffs 0 :triggers])
             [{:parent            (db/find-slug ds "Triggers:Effect")
               :target            (db/find-slug ds "Froda's_Jump_Discontinuity")
               :chance/percentage 100}])
          "Buff triggers"))))

(deftest test-spell-buffs-properties
  (testing "Buffs have certain special properties."
    (let [ds (db/db reconciler)]
      (is (= (get-in (expand-item (db/find-slug ds "Transdimensional_Dodging"))
                     [:buffs 0 :buff/removable]) true) "Removable buff")
      (is (= (get-in (expand-item (db/find-slug ds "Killing_Blow"))
                     [:buffs 0 :buff/removable]) nil) "Not removable buff")
      (is (= (get-in (expand-item (db/find-slug ds "Mark_of_Chthon"))
                     [:buffs 0 :mana/upkeep]) 4) "Buff mana upkeep")
      (is (= (get-in (expand-item (db/find-slug ds "On_Fire"))
                     [:buffs 0 :max/turns]) 12) "Buff cooldown")
      (is (= (get-in (expand-item (db/find-slug ds "Blungecap_Vampirism"))
                     [:buffs 0 :max/turns]) 19) "Limited buff (max turns)")
      (is (= (get-in (expand-item (db/find-slug ds "Lobstery_Toughness"))
                     [:buffs 0 :max/hits]) 5) "Limited buff (max hits)")
      (is (= (get-in (expand-item (db/find-slug ds "Fell_Power"))
                     [:buffs 0 :max/attacks]) 6) "Limited buff (max attacks)")
      (is (= (get-in (expand-item (db/find-slug ds "Tortoise_Maneuver"))
                     [:buffs 0 :prereqs])
             [{:parent (db/find-slug ds "Requires:Requires_Shield")}])
          "Buff requires shield"))))

(deftest test-spell-buffs-stacks
  (testing "Buffs may be stackable."
    (let [ds (db/db reconciler)]
      (is (= (get-in (expand-item (db/find-slug ds "Arctic_Swirly"))
                     [:buffs 0 :max/stacks]) 8) "Buff stackable (max-stacks)")
      (is (= (get-in (expand-item (db/find-slug ds "Touch_of_Midas"))
                     [:buffs 0 :max/stacks]) -1) "Buff stackable (infinite-stacks)")
      (is (= (get-in (expand-item (db/find-slug ds "On_Fire"))
                     [:buffs 0 :max/stacks]) 1) "Buff not stackable (stacksize = 1)")
      (is (= (get-in (expand-item (db/find-slug ds "Mark_of_Chthon"))
                     [:buffs 0 :max/stacks]) 1) "Buff not stackable (stackable = 0)")
      (is (= (get-in (expand-item (db/find-slug ds "Diggle_Flu"))
                     [:buffs 0 :max/stacks]) 1) "Buff not stackable (allowstacking = 0)"))))
