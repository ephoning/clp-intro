(ns clp-intro.peano-v3
  (:require [clojure.core.logic :refer :all]
            [clp-intro.basics :refer :all]))

;; --------------------------------------
;; CLP
;; V3
;; - as we allow more than one LVar, we really expect multiple answers
;; - note: NOT a canonical way of defining <func-name>o functions, as we include
;;   a call to 'run*' (instead of having the caller take care of that), but this allows us
;;   to hide the 'front-peano' call
(defn addo-peano-V3 [x y s]
  (apply from-peano
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
