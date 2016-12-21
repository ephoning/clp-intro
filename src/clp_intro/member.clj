(ns clp-intro.member
  (:require [clojure.core.logic :refer :all :exclude [appendo membero]]))

;; compact
(defn member-v1
  "find in seq + return remainder - functional"
  [x l]
  (cond
    (empty? l) false
    (= x (first l)) l
    :else (member-v1 x (rest l))))

;; spelled out to resemble structure of the relational version
(defn member-v2
  "find in seq + return remainder - functional"
  [x l]
  (cond
    (empty? l) false
    :else (let [a (first l)
                d (rest l)]
            (cond
              (= x a) l
              :else (member-v2 x d)))))


(defn membero-v1
  "find in seq + return remainder - relational"
  [x l out]
  (conde
   [(== '() l) (== false out)]
   [(fresh [a d]
      (== (lcons a d) l)
      (conde
       [(== x a) (== l out)]
       [(membero-v1 x d out)]))]))

;; adding proper negation clause...
(defn membero-v2
  "find in seq + return remainder - relational"
  [x l out]
  (conde
   [(== '() l) (== false out)]
   [(fresh [a d]
      (== (lcons a d) l) ; note: implies l != ()
      (conde
       [(== x a) (== l out)]
       [(!= x a) (membero-v2 x d out)]))]))

;; or, equivalently:
(defn membero-v3
  "find in seq + return remainder - relational"
  [x l out]
  (conde
   [(== '() l) (== false out)]
   [(fresh [a d]
      (== (lcons a d) l) (== x a) (== l out))]
   [(fresh [a d]
      (== (lcons a d) l) (!= x a) (membero-v3 x d out))]))


(def membero membero-v3)
