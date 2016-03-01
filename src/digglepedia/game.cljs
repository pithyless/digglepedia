(ns digglepedia.game
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])
    (:require [digglepedia.logging :as log]
              [digglepedia.game.xml :as xml]
              [digglepedia.util :refer [compact-map]]
              [goog.net.XhrIo :as xhr]
              [clojure.string :as str]
              [cljs.reader :as rdr]
              [cljs.core.async :as async :refer [chan close! >! <!]]))

(def game-data-mods
  [{:mod "Mods:Base" :url "/game-data/initial.edn"}
   {:mod "Mods:Base" :url "/game-data/base/game/text.xml"}
   {:mod "Mods:Base" :url "/game-data/base/game/skillDB.xml"}
   {:mod "Mods:Base" :url "/game-data/base/game/itemDB.xml"}
   {:mod "Mods:Base" :url "/game-data/base/game/spellDB.xml"}
   {:mod "Mods:Base" :url "/game-data/base/game/monDB.xml"}])

(defn GET [url]
  (let [ch (chan 1)]
    (xhr/send url (fn [event] (let [res (-> event .-target .getResponseText)]
                                (go (>! ch res) (close! ch)))))
    ch))

(defn process-raw-data
  [callback {:keys [mod url]} raw]
  (callback
    (map #(compact-map (merge {:game/mod [:slug mod]} %))
         (if (str/ends-with? url ".edn")
           (rdr/read-string raw)
           (xml/parse-xml raw)))))

(defn fetch-and-parse-mods [callback]
  (log/info :fetching-remote-data)
  (let [data (doall (map #(conj % {:chan (GET (:url %))}) game-data-mods))]
    (go
      (<!
        (go-loop [[d & ds] data]
          (when d
            (process-raw-data callback d (<! (:chan d)))
            (recur ds))))
      (callback [[:db/add 0 :ui/loading false]]))))


