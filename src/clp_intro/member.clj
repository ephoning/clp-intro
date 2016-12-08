(ns clp-intro.member
  (:require [clojure.core.logic :refer :all]))

(defn member
  "find in seq + return remainder - functional"
  [x l]
  (cond
    (empty? l) false
    (= x (first l)) l
    :else (member x (rest l))))

(defn membero
  "find in seq + return remainder - relational"
  [x l out]
  (conde
   [(== '() l) (== false out)]
   [(== )]
   [])
)
