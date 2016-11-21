(ns clp-intro.peano)

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
