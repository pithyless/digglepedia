(ns digglepedia.cards.state
  (:require [digglepedia.logging :as log]
            [digglepedia.web.parser :as parser]
            [digglepedia.db :as db]
            [om.next :as om]))

(defonce conn (db/new-database))

(defonce reconciler
  (let [r (parser/new-reconciler conn)]
    (om/transact! r `[(app/load-db)])
    r))
