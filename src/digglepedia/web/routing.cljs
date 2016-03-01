(ns digglepedia.web.routing
  (:require [digglepedia.logging :as log]
            [goog.events :as events]
            [goog.history.EventType :as EventType]))

(defn on-navigate [event]
  (log/debug "NAVIGATE")
  (.debug js/console event))

(defn enable-navigation [history]
  (doto history
    (events/listen EventType/NAVIGATE on-navigate)
    (.setEnabled true)))

