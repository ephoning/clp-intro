(ns clp-intro.append
  (:require [clojure.core.logic :refer :all]))

(defn append
  "append 2 seqs - functional"
  [a b]
  (if (empty? a) b (cons (first a)(append (rest a) b))))

(defn appendo
  "append 2 seqs - relational"
  [l s out]
  (conde
   [(== '() l) (== s out)]
   [(fresh [a d r]
      (== (lcons a d) l)  ; (also implies l != ())
      (== (lcons a r) out)
      (appendo d s r))])) ; make recursive call last to avoid non-termination
                          ; when asking for non-existent answers
