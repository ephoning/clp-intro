(ns clp-intro.peano-v2
  (:require [clojure.core.logic :refer :all]
            [clp-intro.peano :refer :all]))

(defn to-lvar [a aL]
  (== aL (if (keyword? a) aL (to-peano a))))

;; --------------------------------------
;; - we allow more than one LVar => we should expect multiple answers
;; - note: NOT a CANONICAL way of defining o-functions (i.e., it cannot feature within another o-function,
;;   as we include a call to 'run*' (instead of having the caller take care of that), but this allows us
;;   to hide the 'from-peano-nested' call
(defn run*-addo [x y s]
  (apply from-peano-nested
         (run* [q]
           (fresh [xL yL sL]
             ;; unify LVars with themselves (no-op) or ("assign to") passed-in values...
             ;; (in other words: we "promote" the keywords to actual LVars)
             (to-lvar x xL)
             (to-lvar y yL)
             (to-lvar s sL)
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
             (to-lvar x xL)
             (to-lvar y yL)
             (to-lvar s sL)
             ;; actual constraint resolution...
             (appendo xL yL sL)
             (== xL yL)

             ;; unify "result LVar" with the bundled result
             (== q (vector xL yL sL))))))


;; NEXT: Can we hide the to/from Peano marshalling?
