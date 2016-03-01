(ns digglepedia.cards.core
 (:require [digglepedia.logging :as log]
           [devcards.core :as dc :refer-macros [defcard deftest]]
           [digglepedia.cards.game]
           [digglepedia.cards.spells]
           [digglepedia.cards.items]
           [digglepedia.cards.queries]))

(defn start []
  (dc/start-devcard-ui!))
