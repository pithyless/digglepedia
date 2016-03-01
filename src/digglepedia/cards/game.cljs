(ns digglepedia.cards.game
 (:require [digglepedia.logging :as log]
           [digglepedia.db :as db]
           [digglepedia.cards.state :refer [reconciler]]
           [clojure.string :as str]
           [sablono.core :as html :refer-macros [html]]
           [om.next :as om]
           [cljs.core.async :as a :refer [chan close! >! <!]]
           [cljs.test :refer-macros [is testing]]
           [devcards.core :as dc :refer-macros [defcard deftest]]
           [devcards-om-next.core :as don :refer-macros [om-next-root defcard-om-next]]))


;;
;; ----- Database ------
;;

(defn db-views []
  (let [ds (db/db reconciler)]
    {:slugs           (db/entities ds (db/slugs ds))

     :stats/primary   (db/entities ds (db/children ds [:slug "Stats:Primary"]))
     :stats/secondary (db/entities ds (db/children ds [:slug "Stats:Secondary"]))
     :stats/damage    (db/entities ds (db/children ds [:slug "Stats:Damage"]))
     :stats/resist    (db/entities ds (db/children ds [:slug "Stats:Resistance"]))
     :taxanomies      (db/entities ds (db/children ds [:slug "Taxa"]))

     :spell/types     (db/entities ds (db/children ds [:slug "Spells"]))
     :spells          (db/entities ds (db/leafs    ds [:slug "Spells"]))

     :item/types      (db/entities ds (db/children ds [:slug "Items"]))
     :items           (db/entities ds (db/leafs    ds [:slug "Items"]))
     }))

(defcard "# Game Database

There are many kinds of things in our database.")

(defcard "## Statistics"
  (fn []
    (let [pts (db-views)]
      (html
        [:table
         [:tbody
          (for [[k v] pts]
               [:tr
                [:td (str k)]
                [:td (count v)]
                [:td (str (->> v (take 3) (map db/ent-name) (str/join ", ")) "...")]
                ])]]))))

(deftest test-game-statistics
  (testing "Statistics"
    (let [stats (db-views)]
      (is (= (count (:slugs stats)) 1178) "Slugs")
      (is (= (count (:stats/primary stats)) 6) "Primary Stats")
      (is (= (count (:stats/secondary stats)) 25) "Secondary Stats")
      (is (= (count (:stats/damage stats)) 16) "Damages")
      (is (= (count (:stats/resist stats)) 16) "Resistances")
      (is (= (count (:taxanomies stats)) 8) "Taxanomies")
      (is (= (count (:spell/types stats)) 16) "Spell Types")
      (is (= (count (:spells stats)) 465) "Spells")
      (is (= (count (:item/types stats)) 7) "Item Types")
      (is (= (count (:items stats)) 375) "Items"))))

;; TODO - slugs w/o any attributes
