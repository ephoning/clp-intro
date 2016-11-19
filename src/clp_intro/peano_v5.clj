(ns clp-intro.peano-v5
  (:require [clojure.core.logic :refer :all]
            [clp-intro.basics :refer :all]))

;; --------------------------------------
;; CLP
;; V5
;; strategy:
;; - "promote" keyword params to LVars asap to allow combining with other CLP functions
;; - caller to handle to/from Peano representation marshalling?? (UGLY! but unavoidable?)
(defn addo-peano-V5 [x y s]
  (fresh [xL yL sL]
    ;; unify LVars with themselves (no-op) or ("assign to") passed-in values...
    ;; (in other words: we "promote" the keywords to actual LVars)
    (== xL (if (keyword? x) xL x))
    (== yL (if (keyword? y) yL y))
    (== sL (if (keyword? s) sL s))
    ;; actual constraint resolution...
    (appendo xL yL sL)
    ;; unify "result LVar" with the bundled result
    (vector xL yL sL)))

;; notice added constraint: result has to be the sum of 2 equal values
;; the caller takes care of to/from Peano marshalling
(defn example-addo-peano-V5-multiple-constraints [x y s]
  (let [xP (to-peano x)
        yP (to-peano y)
        sP (to-peano s)]
    (run* [q]
      (== q (addo-peano-V5 xP yP sP)))
    ;; (run* [q]
    ;;   (fresh [xL yL]
    ;;     (== xL xP)
    ;;     (== yL yP)
    ;;     (== xL yL) ;; force the numbers that add up to 's' to be equal
    ;;     (== q (addo-peano-V5 xL yL sP))))
    ))

(defn example-addo-peano-V5-multiple-constraints-tst [x y s]
  (run* [q]
    (== q (addo-peano-V5 x y s)))
  ;; (run* [q]
  ;;   (fresh [xL yL]
  ;;     (== xL xP)
  ;;     (== yL yP)
  ;;     (== xL yL) ;; force the numbers that add up to 's' to be equal
  ;;     (== q (addo-peano-V5 xL yL sP))))
  )
