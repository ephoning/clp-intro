(ns clp-intro.append-clp
  (:require [clojure.core.logic :refer :all]))

;; unify lvar with itself (no-op) or ("assign to") passed-in value
;; (in other words: we "promote" a keyword to actual lvar)
(defn to-lvar [a aL]
  (== aL (if (keyword? a) aL (to-peano a))))
