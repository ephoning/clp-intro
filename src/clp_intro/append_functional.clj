(ns clp-intro.append-functional)

(defn append
  " append 2 seqs"
  [a b]
  (letfn [(_append [a b] (if (empty? a) b (_append (rest a) (cons (first a) b))))]
    (_append (reverse a) b)))
