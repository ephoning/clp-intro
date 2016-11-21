(ns clp-intro.basics
  (:require [clojure.core.logic :refer :all]))


;;;
;;; Constraint Logic Programming with core.logic / miniKanren
;;;

;;; --------------------------------------
;;; Peano addition

;; convert natural number to Peano representation
;; (if given a keyword, leave it untouched)
(defn to-peano [n]
  (if (keyword? n) n (if (= n 0) () (cons \S (to-peano (dec n))))))

(defn from-peano-single [p]
  (count p))

;;; deals with flat list
(defn from-peano-flat [p & tail]
  (cons (count p)
        (cond (empty? tail) ()
              true (apply from-peano-flat tail))))

;;; deals with nested list
(defn from-peano-nested [ps & tail]
  (letfn [(_from-peano [l] (if (empty? l) () (cons (from-peano-single (first l)) (_from-peano (rest l)))))]
    (cons (_from-peano ps) (if (empty? tail) () (apply from-peano-nested tail)))))

;; --------------------------------------
;; functional
(defn add-peano [x y]
  (let [xP (to-peano x)
        yP (to-peano y)
        sP (concat xP yP)]
    (from-peano-single sP)))

;; --------------------------------------
;; lvar "propagation" experiments

;; constructing an lvar in a nested call and passing it up/out to the caller

(defn unify-lvar-up
  "the fresh' 'action' cannot emit an lvar"
  []
  (fresh [vL]
         (== vL 42)
         vL))

(defn lvar-up-A
  "WRONG: == does NOT nest; it is an 'action' NOT resulting in an lvar"
  []
  (run* [q]
        (== q (unify-lvar-up))))

(defn lvar-up-B
  "WRONG AGAIN: vL is NOT a 'global' that can escape its scope"
  []
  (run* [q]
        (unify-lvar-up)
        ;(== q vL) does not compile...
        ))

;; ----------------

;; constructing an lvar in a caller and passing it down/into a nested call

(defn unify-lvar-down-A
  "lvar unification action + lvar argument returned"
  [xL]
  (== xL 42)
  xL)

(defn lvar-down-A
  "WRONG: vL is NOT reified to '42', but still unreified"
  []
  (run* [q]
        (fresh [vL]
               (== q (unify-lvar-down-A vL)))))

;; ----------------

(defn unify-lvar-down-B
  "lvar unification action only"
  [xL]
  (== xL 42))

(defn lvar-down-B
  "== does NOT nest; it is an 'action' NOT resulting in an lvar"
  []
  (run* [q]
        (fresh [vL]
               (== q (unify-lvar-down-B vL)))))

(defn lvar-down
  "correct use of '==' in a sequence of unification"
  []
  (run* [q]
        (fresh [vL]
               (== q vL)
               (unify-lvar-down-B vL))))

