(ns digglepedia.web.core
 (:require [digglepedia.logging :as log]
           [goog.dom :as gdom]
           [om.next :as om]
           [devcards.core] ;; TODO: figure out how to remove this
           [devtools.core :as devtools] ;; TODO: only for development
           [digglepedia.db :as db]
           [digglepedia.web.routing :as routing]
           [digglepedia.web.ui.app :refer [App]]
           [digglepedia.web.parser :as parser])
 (:import [goog History]))

;; (log/set-level! :fine)
(log/set-level! :warning)

(devtools/enable-feature! :sanity-hints)
(devtools/install!)

(defonce history (History.))

(defonce conn (db/new-database))

(defonce reconciler (parser/new-reconciler conn))

(defn ^:export start []
  (enable-console-print!)
  (routing/enable-navigation history)
  (when-let [node (gdom/getElement "app")]
    (om/transact! reconciler `[(app/load-db)])
    (om/add-root! reconciler App node)))

(defn ^:export reload []
  (enable-console-print!)
  (when-let [node (gdom/getElement "app")]
    (om/add-root! reconciler App node)))
