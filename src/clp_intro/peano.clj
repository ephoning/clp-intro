(ns clp-intro.peano
  (:require [clojure.core.logic :refer :all]))

;; convert natural number to Peano representation
;; (if given a keyword, leave it untouched)
(defn to-peano [n]
  "convert natural number to peano representation (but leave keywords as-is)"
  (if (keyword? n) n (if (= n 0) () (cons \S (to-peano (dec n))))))

(defn from-peano [p]
  "convert potentially nested peano representation(s) to natural number(s)"
  (cond
    (empty? p) 0
    (seq? (first p)) (cons (from-peano (first p)) (if (empty? (rest p)) () (from-peano (rest p))))
    :else (count p)))


;; --------------------------------------
;; (functional)

(defn add-peano [x y]
  (let [xP (to-peano x)
        yP (to-peano y)
        sP (concat xP yP)]
    (from-peano sP)))

;; examples:
;; clp-intro.peano> (from-peano (to-peano 42)) ; 42
;; clp-intro.peano> (from-peano `(~(to-peano 42))) ; (42)

;; --------------------------------------
;; (relational helpers)
;;
(defn to-lvar [v vL]
  "unify lvar with itself (no-op) or passed-in value
  (this, in effect, 'promotes' a keyword to actual lvar)"
  (== vL (if (keyword? v) vL v)))

;; --------------------------------------
;; (relational)

(defn addo-peano [x y s]
  (run* [q]
    (fresh [xL yL sL]
      (to-lvar x xL)
      (to-lvar y yL)
      (to-lvar s sL)
      (appendo xL yL sL)
      (== q (list xL yL sL)))))

(defn subo-peano [x y d]
  (let [r (addo-peano d y x)]
    (loop [l r accu ()]
      (if (empty? l) accu
          (recur (rest l) (cons (let [[d y x] (first l)] `(~x ~y ~d)) accu))))))

;; examples:
;; (from-peano (addo-peano (to-peano 1) (to-peano 2) :s))  ; ((1 2 3))
;; (from-peano (addo-peano :x :y (to-peano 3)))            ; ((0 3 3) (1 2 3) (2 1 3) (3 0 3))

;; (from-peano (subo-peano (to-peano 3) (to-peano 1) :d))  ; ((3 1 2))
;; (from-peano (subo-peano (to-peano 3) :a :b))            ; ((3 0 3) (3 1 2) (3 2 1) (3 3 0))
;; --------------------------------------
;;
