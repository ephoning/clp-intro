(ns clp-intro.append-clp
  (:require [clojure.core.logic :refer :all]))

;; unify lvar with itself (i.e., "promote" keyword to lvar) or passed-in value
(defn to-lvar [a aL]
  (== aL (if (keyword? a) aL a)))
