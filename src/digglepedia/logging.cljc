(ns digglepedia.logging
  (:refer-clojure :exclude [time])
  (:require #?(:clj  [clojure.tools.logging :as log]
               :cljs [goog.log :as glog]))
  #?(:cljs (:import goog.debug.Console)))

#?(:cljs
   (def logger
     (glog/getLogger "diggle")))

#?(:cljs
   (def levels {:severe goog.debug.Logger.Level.SEVERE
                :warning goog.debug.Logger.Level.WARNING
                :info goog.debug.Logger.Level.INFO
                :config goog.debug.Logger.Level.CONFIG
                :fine goog.debug.Logger.Level.FINE
                :finer goog.debug.Logger.Level.FINER
                :finest goog.debug.Logger.Level.FINEST}))

#?(:cljs
   (defn log-to-console! []
     (.setCapturing (goog.debug.Console.) true)))

#?(:cljs
   (defn set-level! [level]
     (.setLevel logger (get levels level (:info levels)))))

(defn fmt [msgs]
  (apply str (interpose " " (map pr-str msgs))))

(defn info [& s]
  (let [msg (fmt s)]
    #?(:clj  (log/info msg)
       :cljs (glog/info logger msg))))

;;
;; TODO: :cljs (.debug js/console s))))
;; https://github.com/binaryage/cljs-devtools/issues/14
;;
(defn debug [& s]
  (let [msg (fmt s)]
    #?(:clj  (log/debug msg)
       :cljs (glog/fine logger (str "[DEBUG] " msg)))))

(defn error [& s]
  (let [msg (fmt s)]
    #?(:clj (log/error msg)
       :cljs (glog/error logger msg))))
