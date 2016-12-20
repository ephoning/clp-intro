(ns clp-intro.append
  (:require [clojure.core.logic :refer :all :exclude[appendo]]))

;; compact
(defn append-v1
  "append 2 seqs - functional"
  [l s]
  (if (empty? l) s
      (cons (first l)
            (append-v1 (rest l) s))))

;; spelled out to resemble structure of the relational version
(defn append-v2
  "append 2 seqs - functional"
  [l s]
  (if (empty? l) s
      (let [a (first l)
            d (rest l)
            r (append-v2 d s)]
        (cons a r))))

(defn appendo
  "append 2 seqs - relational"
  [l s out]
  (conde
   [(== '() l) (== s out)]
   [(fresh [a d r]
      (== (lcons a d) l)   ; a == (first l), d == (rest l)
      (== (lcons a r) out) ; out == (a . r)
      (appendo d s r))]))  ; r == ( (rest l) s)
