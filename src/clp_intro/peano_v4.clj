(ns clp-intro.peano-v4
  (:require [clojure.core.logic :refer :all]
            [clp-intro.peano :refer :all]))

;; unify lvar with itself (no-op) or ("assign to") passed-in value
;; (in other words: we "promote" a keyword to actual lvar)
(defn to-lvar [a aL]
  (== aL (if (keyword? a) aL (to-peano a))))


;; --------------------------------------
;; strategy:
;; - use a "nested run*" to force reification of the peano addition into
;;   triplets of regular natural numbers
;; - deconstruct this reified result using membero/firsto/resto to allow further
;;   handling / constraint enforcement
(defn addo-peano [x y s]
  (apply from-peano-nested
         (run* [q]
           (fresh [xL yL sL]
             (to-lvar x xL)
             (to-lvar y yL)
             (to-lvar s sL)
             ;; actual constraint resolution...
             (appendo xL yL sL)
             ;; unify result lvar with the bundled result
             (== q (list xL yL sL))))))

;; get those numbers x and y that are the same that add up to s
;; note:
;; - the result of addo-peano is a reified list of triplets that, in order to feature
;; - in further constraint processing has to be deconstructed into lvars again
(defn addo-peano-samo [x y s]
  (run* [q]
    (fresh [addoL xL yL accuL tripletsL pairsL]
      (== addoL (addo-peano x y s))
      (membero tripletsL addoL) ;; "break up"" addoL into triplets"
      (firsto tripletsL xL)     ;; extract the 'car' from the triplet
      (resto tripletsL pairsL)
      (firsto pairsL yL)        ;; extract the cadr from the triplet
      (== xL yL)                ;; force equality
      (== xL q))))              ;; retain those that match the constraints
