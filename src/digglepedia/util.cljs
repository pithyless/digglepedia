(ns digglepedia.util
    (:require [clojure.string :as str]))

(defn getx [m k]
  (let [e (get m k ::sentinel)]
    (if-not (= e ::sentinel) e (throw (ex-info "Missing required key" {:map m :key k})))))

(defn getx-in [m ks]
  (reduce getx m ks))

(defn parse-int [s]
  (let [n (js/parseInt s)]
    (assert (not (js/isNaN n)) (str "Not a number: " s))
    n))

(defn parse-float [s]
  (let [n (js/parseFloat s)]
    (assert (not (js/isNaN n)) (str "Not a float: " s))
    n))

(defn round [s]
  (let [n (parse-float s)]
    (js/Math.round n)))

(defn compact-map [m]
  (->> m (filter second) (into {})))

(defn lower-case-keyword [k]
  (->> [(namespace k) (name k)]
       (remove nil?)
       (map str/lower-case)
       (str/join "/")
       (keyword)))

