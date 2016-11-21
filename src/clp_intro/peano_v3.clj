(ns clp-intro.peano-v3
  (:require [clojure.core.logic :refer :all]
            [clp-intro.basics :refer :all]))

;; --------------------------------------
;; - as we allow more than one LVar, we really expect multiple answers
;; - note: NOT a CANONICAL way of defining o-functions (i.e., it cannot feature within another o-function,
;;   as we include a call to 'run*' (instead of having the caller take care of that), but this allows us
;;   to hide the 'from-peano-nested' call
(defn run*-addo [x y s]
  (apply from-peano-nested
         (run* [q]
           (fresh [xL yL sL]
             ;; unify LVars with themselves (no-op) or ("assign to") passed-in values...
             ;; (in other words: we "promote" the keywords to actual LVars)
             (== xL (if (keyword? x) xL (to-peano x)))
             (== yL (if (keyword? y) yL (to-peano y)))
             (== sL (if (keyword? s) sL (to-peano s)))
             ;; actual constraint resolution...
             (appendo xL yL sL)
             ;; unify "result LVar" with the bundled result
             (== q (vector xL yL sL))))))

;; with added constraint: arguments to 'add' must be the same
(defn run*-addo-same [x y s]
  (apply from-peano-nested
         (run* [q]
               (fresh [xL yL sL]
                      ;; unify LVars with themselves (no-op) or ("assign to") passed-in values...
                      ;; (in other words: we "promote" the keywords to actual LVars)
                      (== xL (if (keyword? x) xL (to-peano x)))
                      (== yL (if (keyword? y) yL (to-peano y)))
                      (== sL (if (keyword? s) sL (to-peano s)))
                      ;; actual constraint resolution...
                      (appendo xL yL sL)
                      (== xL yL)
                      ;; unify "result LVar" with the bundled result
                      (== q (vector xL yL sL))))))


;; NEXT: Can we hide the to/from Peano marshalling?