(ns digglepedia.web.styles
  (:refer-clojure :exclude [+ - * /])
  (:require [garden.def :refer [defstyles defrule defkeyframes]]
            [garden.core :refer [css]]
            [garden.units :refer [px em]]
            [garden.color :as color :refer [hsl rgb]]
            [garden.arithmetic :refer [+ - * /]]
            [gardener.images :as images]
            [gardener.respond :as respond :refer [breakpoints]]
            [gardener.resets :as resets]))

(def font-families
  {:serif ["Bitter" "Baskerville" "Georgia" "Times" "serif"]
   :sans  ["Lato" "\"Open Sans\"" "Avenir" "Helvetica" "sans-serif"]
   :mono  ["Inconsolata" "Menlo" "Courier" "monospace"]})

(defstyles reset
  resets/reset-common-selectors
  [:*
   {:box-sizing :border-box
    :min-width 0
    :min-height 0}])

(defstyles page
  [:html :body :#app
   {:margin 0
    :padding 0
    :height "100%"}])

(defstyles typography
  (let [size (fn [f l] {:font-size (px f)
                        :line-height (px l)})]
    [[:#app
      (merge {:font-family (:serif font-families)
              :color "#804d15"
              :font-weight "400"}
            (size 12 20))]
     [:.logo
      (merge {:font-weight "700"
              :color "#aa7439"
              :margin-right (em 1)}
             (size 24 28))]
     [:.title
      (merge {:font-weight "700"
              :color "#804d15"}
             (size 16 24))]
     [:.section
      (merge {:margin-top (em 0.5)
              :font-weight "700"
              :color "#804d15"}
             (size 14 22))]]))

(defstyles boxes
  [[:.display-flex
    {:display :flex}]])

(defstyles images
  [[:.s
    {:background-image "url(\"/game-data/sprite1.png\")"
     :background-repeat :no-repeat}]])

(defstyles layout
  [
   [:.layout-left :.layout-mid
    {:background-color "#f7f7f7"}]
   [:.layout-right
    {:background-color "#fff"}]
   [:.layout-left-head :.layout-mid-head
    {:background-color "#f7f7f7"}]
   [:.layout-left-body :.layout-mid-body :.layout-right-body
    {:padding (em 1)}]
   ])

(def index
  (reduce merge
    (reverse [reset page layout typography boxes images])))
