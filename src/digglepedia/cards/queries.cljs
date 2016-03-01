(ns digglepedia.cards.queries
  (:require [digglepedia.logging :as log]
            [digglepedia.db :as db]
            [digglepedia.cards.state :refer [reconciler]]
            [digglepedia.web.ui.app :as ui]
            [clojure.string :as str]
            [clojure.data]
            [sablono.core :as html :refer-macros [html]]
            [om.next :as om]
            [cljs.core.async :as a :refer [chan close! >! <!]]
            [cljs.test :refer-macros [is testing]]
            [devcards.core :as dc :refer-macros [defcard defcard-doc deftest]]
            [devcards-om-next.core :as don :refer-macros [om-next-root defcard-om-next]]))

(defn test-reconciler [state]
  (let [reader (fn [{:keys [state]} key _] {:value (get @state key)})]
    (om/reconciler
     {:state  state
      :parser (om/parser {:read reader})})))

(defn- props-ui-slug [ui slug]
  (let [parser (-> reconciler :config :parser)
        query  (om/get-query ui)
        props  (db/pull-slug (db/db reconciler) slug query)]
    props))


(defcard-doc "# Data Query

Components define a static data query. This query must
describe *all* data that the render function may need."
  (dc/mkdn-pprint-source ui/Effect)

  "We build a UI tree, by composing components together. The data query also is composed."
  (dc/mkdn-pprint-source ui/Spell)

  "Which expands to:"
  (om/get-query ui/EffectsGroup)

  "A render function may not ask to fetch more data. That's why the query
must include all possibly necessary data, including any branching logic.
Notice that not every attribute exists for each Effect."
  (dc/mkdn-pprint-source ui/Effect)

  "Eventually, we reach the root of the UI tree."
  (dc/mkdn-pprint-source ui/App)

  "Which, when expanded, tells us about everything we need to render the entire page."
  (om/get-query ui/App))

(defcard-doc "## Spell Effects")

(deftest test-spell-effects
  (testing "Spell Effects"
    (let [props (props-ui-slug ui/EffectsGroup "Acid_Burn")]
      (is (= (get-in props [:effects 0 :parent :slug])
             "Effects:Starts_Bleeding")
          "Bleeds"))
    (let [props (props-ui-slug ui/EffectsGroup "Ignition_Bolt")]
      (is (= (get-in props [:effects 0 :parent :slug])
             "Effects:Burns_Target")
          "Burns"))
    (let [props (props-ui-slug ui/EffectsGroup "It_Belongs_In_A_Museum")]
      (is (= (get-in props [:effects 0 :parent :slug])
             "Effects:Consumes_Item")
          "Consumes Item"))
    (let [props (props-ui-slug ui/EffectsGroup "Blungecap_Vampirism_Attack")]
      (is (= (get-in props [:effects 0 :parent :slug])
             "Effects:Drains_Life")
          "Drains Life"))
    (let [props (props-ui-slug ui/EffectsGroup "Midas")]
      (is (= (get-in props [:effects 0 :parent :slug])
             "Effects:Midas")
          "Midas"))
    (let [props (props-ui-slug ui/EffectsGroup "Flamefield_2")]
      (is (= (get-in props [:effects 0 :parent :slug])
             "Effects:Mine")
          "Mine"))
    (let [props (props-ui-slug ui/EffectsGroup "Monster_Toss")]
      (is (= (get-in props [:effects 0 :parent :slug])
             "Effects:Performs_Melee_Attack")
          "Melee Attack"))
    (let [props (props-ui-slug ui/EffectsGroup "Lightning_Blast")]
      (is (= (get-in props [:effects 2 :parent :slug])
             "Effects:Paralyzes")
          "Paralyzes"))
    (let [props (props-ui-slug ui/EffectsGroup "Narcosomatic_Induction")]
      (is (= (get-in props [:effects 1 :parent :slug])
             "Effects:Puts_to_Sleep")
          "Puts to Sleep"))
    (let [props (props-ui-slug ui/EffectsGroup "Froda's_Jump_Discontinuity")]
      (is (= (get-in props [:effects 0 :parent :slug])
             "Effects:Randomly_Teleports")
          "Randomly Teleports")))
  (testing "Spell Effect Properties"
    (let [props (props-ui-slug ui/EffectsGroup "Acid_Burn")]
      (is (= (get-in props [:effects 0 :affects/caster])
             true)
          "Affects Caster"))
    (let [props (props-ui-slug ui/EffectsGroup "Blungecap_Vampirism_Attack")]
      (is (= (get-in props [:effects 0 :affects/taxa :slug])
             "Taxa:Animal")
          "Affects Taxanomy"))))


(deftest test-spell-effects-damages
  (testing "Damages"
    (let [props (props-ui-slug ui/EffectsGroup "Aethereal_Missile")]
      (is (= (get-in props [:effects 0 :parent :slug])
             "Effects:Damages"))
      (is (= (get-in props [:effects 0 :target :slug])
             "Aethereal_Damage"))
      (is (= (get-in props [:effects 0 :target :icon])
             "dmg_aethereal.png"))
      (is (= (get-in props [:effects 0 :amount])
             7))))
  (testing "Damages (with boost)"
    (let [props (props-ui-slug ui/EffectsGroup "Aethereal_Missile")]
      (is (= (get-in props [:effects 1 :target :slug])
             "Blasting_Damage"))
      (is (= (get-in props [:effects 0 :amount/modifier :icon])
             "stat_magicpower.png"))
      (is (= (get-in props [:effects 1 :amount])
             3))
      (is (= (get-in props [:effects 0 :amount/modifier :slug])
             "Magic_Power"))
      (is (= (get-in props [:effects 0 :amount/boost])
             0.1)))))


(defcard-om-next effects-group-damages-with-boost
  ui/EffectsGroup
  (test-reconciler (props-ui-slug ui/EffectsGroup "Aethereal_Missile")))

(defcard-om-next effects-group-bleeds
  ui/EffectsGroup
  (test-reconciler (props-ui-slug ui/EffectsGroup "Acid_Burn")))

(defcard-om-next effects-group-burns
  ui/EffectsGroup
  (test-reconciler (props-ui-slug ui/EffectsGroup "Ignition_Bolt")))

(defcard-om-next effects-group-consumes-item
  ui/EffectsGroup
  (test-reconciler (props-ui-slug ui/EffectsGroup "It_Belongs_In_A_Museum")))

(defcard-om-next effects-group-drains
  ui/EffectsGroup
  (test-reconciler (props-ui-slug ui/EffectsGroup "Blungecap_Vampirism_Attack")))

(defcard-om-next effects-group-midas
  ui/EffectsGroup
  (test-reconciler (props-ui-slug ui/EffectsGroup "Midas")))

(defcard-om-next effects-group-mine
  ui/EffectsGroup
  (test-reconciler (props-ui-slug ui/EffectsGroup "Flamefield_2")))

(defcard-om-next effects-group-melee
  ui/EffectsGroup
  (test-reconciler (props-ui-slug ui/EffectsGroup "Monster_Toss")))

(defcard-om-next effects-group-paralyze
  ui/EffectsGroup
  (test-reconciler (props-ui-slug ui/EffectsGroup "Lightning_Blast")))

(defcard-om-next effects-group-sleep
  ui/EffectsGroup
  (test-reconciler (props-ui-slug ui/EffectsGroup "Narcosomatic_Induction")))

(defcard-om-next effects-group-teleports
  ui/EffectsGroup
  (test-reconciler (props-ui-slug ui/EffectsGroup "Froda's_Jump_Discontinuity")))

;; TODO - knocks back - Defensive Bash


(deftest test-spell-attributes
  (testing "Spell Attributes"
    (let [props (props-ui-slug ui/Spell "Duck_And_Cover!")]
      (is (= (get-in props [:slug])
             "Duck_And_Cover!")
          "Slug"))
    (let [props (props-ui-slug ui/Spell "Duck_And_Cover!")]
      (is (= (get-in props [:description])
             "Fight smarter, not better! Can increase your defense for a while after you get hit.")
          "Description"))
    (let [props (props-ui-slug ui/Spell "Duck_And_Cover!")]
      (is (= (get-in props [:icon])
             "skills/skill_shieldbearer_small.png")
          "Icon"))
    (let [props (props-ui-slug ui/Spell "Duck_And_Cover!")]
      (is (= (get-in props [:cooldown/turns])
             48)
          "Cooldown"))))

(defcard-om-next spell-duck-and-cover
  ui/Spell
  (test-reconciler (props-ui-slug ui/Spell "Duck_And_Cover!")))

(defcard-om-next spell-defensive-bash
  ui/Spell
  (test-reconciler (props-ui-slug ui/Spell "Defensive_Bash")))
