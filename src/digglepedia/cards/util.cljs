(ns digglepedia.cards.util
  (:require [digglepedia.logging :as log]
            [digglepedia.cards.state :refer [reconciler]]
            [digglepedia.db :as db]
            [sablono.core :as html :refer-macros [html]]
            [clojure.data]))

;;
;; ----- Utils ------
;;

(defn expand-item [item]
  (reduce merge (into {} item)
          (for [[k v] (select-keys item [:buffs :prereqs :effects :triggers])]
            {k (mapv expand-item v)})))

(defn make-slug-card [slug]
  (fn []
    (let [item (db/find-slug (db/db reconciler) slug)]
      (html
       [:div (db/ent-name item)]))))


(defn debug-diff [a b]
  (log/debug :DIFF (clojure.data/diff a b)))
