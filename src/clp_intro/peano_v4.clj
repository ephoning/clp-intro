(ns clp-intro.peano-v4
  (:require [clojure.core.logic :refer :all]
            [clp-intro.basics :refer :all]))

;; --------------------------------------
;; CLP
;; V4
;; - the to/from peano marshalling is now fully hidden AND without embedded 'run*' call
(defn addo-peano-V4 [x y s]
  (fresh [xL yL sL]
    ;; unify LVars with themselves (no-op) or ("assign to") passed-in values...
    ;; (in other words: we "promote" the keywords to actual LVars)
    (== xL (if (keyword? x) xL (to-peano x)))
    (== yL (if (keyword? y) yL (to-peano y)))
    (== sL (if (keyword? r) sL (to-peano s)))
    ;; actual constraint resolution...
    (appendo xL yL sL)
    ;; unify "result LVar" with the bundled result
    (vector (from-peano-single xL) (from-peano-single yL) (from-peano-single sL))))

;; OK: example of using the addo-peano CLP function inside a regular function
(defn example-addo-peano-V4-single-constraint [x y s]
  (run* [q]
    (== q (addo-peano-V4 x y s))))

;; FAIL: attempt at using the addo-peano CLP function with added constraint(s)
(defn example-addo-peano-V4-multiple-constraints [x y s]
  (run* [q]
    (fresh [xL yL]
      (== xL x)
      (== yL y)
      (== xL yL) ;; force the numbers that add up to 'r' to be equal
      (== q (addo-peano-V4 xL yL s)))))
