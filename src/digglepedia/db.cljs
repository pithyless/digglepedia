(ns digglepedia.db
    (:refer-clojure :exclude [descendants ancestors])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])
    (:require [digglepedia.logging :as log]
              [om.next :as om]
              [datascript.core :as d]
              [clojure.string :as str]
              [cljs.core.async :as async :refer [chan close! >! <!]]))

;; ----- Schema ------

(def APP 0)

(def schema-documentation
  {:slug "[string] Unique identity in the sytem (expansion, item, spell, stat, ability, etc)."
   :parent "[:ident :slug] Link to help model a hierarchy."
   :target "[:ident :slug] The target of an effect, spell, etc."

   :artifact/quality "[integer] Number of random qualities that can be applied to item."
   :description "[string] A quip about the entity in question, shown in game."
   :icon "[string] A path to the item's mugshot (size usually 16x16, 32x32, or 64x64)."
   :item/price "[integer] Number of zorkmids something costs."
   :game/mod "[:ident :slug] Link to what expansion it comes from."
   :level "[integer] On what floor you are most likely to trip over item."

   :prereqs "[[ref]] A collection of requirements that must be met first."
   :effects "[[ref]] A collection of resulting effects."
   :buffs "[[ref]] A collection of resulting buffs."
   :triggers "[[ref]] A collection of triggers that may result."

   :affects/caster "[bool] Can you shoot yourself in the foot?"
   :affects/self "[bool] Care to drug yourself?"
   :affects/corpses "[bool] Does it apply to dead things?"
   :affects/taxa "[:ident :slug] Only applies to certain taxanomy."
   :amount "[integer] How much of something."
   :amount/boost "[float] Amount is changed this much by :amount/modifier."
   :amount/min   "[integer] Minimum amount, after all modifiers are applied."
   :amount/modifier "[:ident :slug] Amount is affected by this stat."
   :chance/percentage "[integer] The chance (0-100%) of triggering said effect."

   :buff/removable "[bool] Can be removed at any time."
   :mana/upkeep "[integer] Costs this much mana per turn."
   :after/turns "[integer] Triggers after a certain number of turns."

   :cooldown/turns "[integer] Number of turns one must wait before repeating."
   :max/turns "[integer] Lasts for number of turns."
   :max/hits "[integer] Number of hits one can take before effect is lost."
   :max/attacks "[integer] Number of attacks one can make before effect is lost."
   :max/stacks "[integer] Maximum number of times this effect can stack."
   :irresistable "[bool] Cannot be resisted if true."

   :primary/buff    "[integer] Unique id for primary stat that affects this buff."
   :primary/scale   "[integer] Unique id for primary stat that scales this effect."
   :secondary/buff  "[integer] Unique id for secondary stat that affects this buff."
   :secondary/scale "[integer] Unique id for secondary stat that scales this effect."


   ;; APP
   :ui/loading "[bool] Show loading screen?"
   })


(def schema
  {:slug            {:db/unique :db.unique/identity}
   :parent          {:db/valueType :db.type/ref}
   :game/mod        {:db/valueType :db.type/ref}

   :primary/buff    {:db/unique :db.unique/identity}
   :primary/scale   {:db/unique :db.unique/identity}
   :secondary/buff  {:db/unique :db.unique/identity}
   :secondary/scale {:db/unique :db.unique/identity}

   :target          {:db/valueType :db.type/ref}
   :amount/modifier {:db/valueType :db.type/ref}
   :affects/taxa    {:db/valueType :db.type/ref}

   :buffs           {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many
                     :db/isComponent true}
   :prereqs         {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many
                     :db/isComponent true}
   :effects         {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many
                     :db/isComponent true}
   :triggers        {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many
                     :db/isComponent true}
   })

(def seed-data
  [[:db/add APP :ui/loading true]])

(defn new-database []
  (let [conn (d/create-conn schema)]
    (d/transact! conn seed-data)
    conn))

;; ----- DS utils -----

(def db-with d/db-with)
(def touch d/touch)
(def q' d/q)

(defn db [conn]
  (cond
    (d/conn? conn) (d/db conn)
    (om/reconciler? conn) (-> conn :config :state d/db)))

(defn entity [eid ds]
  (assert (not (nil? eid)))
  (assert (not (nil? ds)))
  (d/entity ds eid))

(defn entities [ds eids]
  (map #(entity % ds) eids))

;; ----- Rules ------

(def rules
  '[[(descendants ?e ?r) [?e :parent ?r]]
    [(descendants ?e ?r) [?e :parent ?p] (descendants ?p ?r)]
    [(non-leafs ?e ?r) (descendants ?e ?r) [_ :parent ?e]]])


;; ----- Updates -----

(defn update-app [conn kvs]
  (d/transact! conn (for [[k v] kvs] [:db/add APP k v])))

;; ----- Reads -----

(defn find-app [ds]
  (entity APP ds))

(defn find-slug [ds slug]
  (some->
    '[:find ?e .
      :in $ ?slug
      :where [?e :slug ?slug]]
    (d/q ds slug)
    (entity ds)))

(defn pull-many [ds query eids]
  (d/pull-many ds query eids))

(defn pull-slug [ds slug query]
  (some->> slug
           (d/q '[:find ?e .
                  :in $ ?slug
                  :where [?e :slug ?slug]] ds)
           (d/pull ds query)))

(defn slugs [ds]
  (-> '[:find [?e ...]
        :where [?e :slug]]
      (d/q ds)))

(defn descendants [ds root]
  (-> '[:find [?e ...]
        :in $ % ?r
        :where (descendants ?e ?r)]
      (d/q ds rules root)))

(defn children [ds root]
  (-> '[:find [?e ...]
        :in $ ?r
        :where [?e :parent ?r]]
      (d/q ds root)))

(defn non-leafs [ds root]
  (-> '[:find [?e ...]
        :in $ % ?r
        :where (non-leafs ?e ?r)]
      (d/q ds rules root)))

(defn leafs [ds root]
  (-> (descendants ds root)
      (set)
      (clojure.set/difference (non-leafs ds root))))

;; ----- Entities -----

(defn ent-name [ent]
  (assert (not (nil? ent)))
  (-> (:slug ent)
      (str/replace "_" " ")
      (str/replace-first #"\w+:(.*)" "$1")))
