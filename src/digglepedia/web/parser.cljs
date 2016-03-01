(ns digglepedia.web.parser
 (:require [digglepedia.logging :as log]
           [digglepedia.db :as db]
           [digglepedia.game :as game]
           [om.next :as om]))

(defmulti readf (fn [{:keys [target]} key _]
                    (log/info :readf key target)
                    [key target]))

(defmulti mutate (fn [{:keys [target]} key _]
                    (log/info :mutate key target)
                    [key target]))


(defmethod readf [:ui/loading nil] [{:keys [state]} _ _]
  {:value (-> state (db/db) (db/find-app) :ui/loading)})

(defmethod readf [:ui/filtered-slugs nil] [{:keys [state query]} _ _]
  (let [ds    (db/db state)
        slugs (->> [:slug "Spells"]
                   (db/leafs ds)
                   (take 6)
                   (db/pull-many ds query))]
    {:value slugs}))

(defmethod readf [:ui/active-slug nil] [{:keys [state query]} _ _]
  {:value (db/pull-slug (db/db state) "Defensive_Bash" query)})

(defmethod readf [:agg/slugs-count nil] [{:keys [state]} _ _]
  {:value (count (db/slugs (db/db state)))})

(defmethod readf [:slugs/all nil] [{:keys [state]} _ _]
  {:value (db/slugs (db/db state))})


;; TODO - think about better way to handle local/remote reads
(defmethod readf [:ui/loading :remote] [_ _ _] nil)
(defmethod readf [:ui/active-slug :remote] [_ _ _] nil)
(defmethod readf [:ui/filtered-slugs :remote] [_ _ _] nil)
(defmethod readf [:agg/slugs-count :remote] [_ _ _] nil)
(defmethod readf [:slugs/all :remote] [_ _ _] nil)


(defmethod mutate [`app/load-db nil] [{:keys [state]} _ _]
  {:value {:keys [:ui/loading]}
   :action (db/update-app state {:ui/loading true})})

(defmethod mutate [`app/load-db :remote] [{:keys [state]} key _]
  {:remote true})


(defn send-fn [{:keys [:remote]} cb]
  (log/info :send remote)
  (case (get-in (om/query->ast remote) [:children 0 :dispatch-key])
    'app/load-db (game/fetch-and-parse-mods cb)))

(defn merge-fn
  [{:keys [state]} pure-state response]
  (log/info :merge [(first response) "..."])
  {:keys [] :next (db/db-with pure-state response)})

(defn new-reconciler [conn]
  (om/reconciler
   {:state  conn
    :send   send-fn
    :merge  merge-fn
    :parser (om/parser {:read readf :mutate mutate})}))
