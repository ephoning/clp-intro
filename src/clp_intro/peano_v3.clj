(ns clp-intro.peano-v3
  (:require [clojure.core.logic :refer :all]
            [clp-intro.peano :refer :all]))

;; unify lvar with itself (no-op) or ("assign to") passed-in value
;; (in other words: we "promote" a keyword to actual lvar)
(defn to-lvar [a aL]
  (== aL (if (keyword? a) aL (to-peano a))))

;; - takes REGULAR vars, NOT lvars!! => CANNOT COMPOSE with other o-function inside run* or fresh scope
;; - the lvar 'r' has to be passed-in from the outer scope so it can be unified with the result
(defn addo [x y s r]
  (fresh [xL yL sL]
    (to-lvar x xL)
    (to-lvar y yL)
    (to-lvar s sL)
    (appendo xL yL sL)
    (== r (vector xL yL sL))))

;; NOTE: ONLY the result of 'run* [q]' is a reified / non-lvar value =>
;; we cannot marshall the nested list of Peano numbers to regular integers *INSIDE' the body of run*
(defn run*-addo [x y s]
  (apply from-peano-nested
         (run* [qL]
           (addo x y s qL))))
