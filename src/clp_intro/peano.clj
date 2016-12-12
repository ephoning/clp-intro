(ns clp-intro.peano
  (:require [clojure.core.logic :refer :all]
            [clojure.string :as str]
            :reload))

(defn any-val? [x]
  (str/starts-with? "_" (str x)))

;; convert natural number to Peano representation
;; (if given a keyword, leave it untouched)
(defn n->peano [n]
  "convert natural number to peano representation (but leave keywords as-is)"
  (if (keyword? n) n (if (= n 0) () (cons \S (n->peano (dec n))))))

(defn peano? [p]
  (and (seq? p) (or (empty? p) (= (first p) \S))))

(defn peano->n [ps]
  "convert potentially nested peano representation(s) to natural number(s)"
  (cond (peano? ps) (count ps)
        (symbol? ps) :any
;;        (any-val? ps) :any
        :else (cons (peano->n (first ps)) (if (empty? (rest ps)) () (peano->n (rest ps))))))

;; examples:
;; clp-intro.peano> (from-peano (to-peano 42)) ; 42
;; clp-intro.peano> (from-peano `(~(to-peano 42))) ; (42)

;; --------------------------------------
;; (functional)

(defn add-peano-f [x-p y-p]
  (concat x-p y-p))

(defn add-f [x y]
  (peano->n (add-peano-f (n->peano x) (n->peano y))))

(defn mult-peano-f [x-p y-p]
 (loop [c x-p i y-p a ()]
   (cond (empty? c) a
         :else (recur (rest c) i (add-peano-f i a)))))

(defn mult-f [x y]
  (peano->n (mult-peano-f (n->peano x) (n->peano y))))


;; --------------------------------------
;; (relational helpers)
;;
(defn to-lvar [v vL]
  "unify lvar with itself (no-op) or passed-in value
  (this, in effect, 'promotes' a keyword to actual lvar)"
  (== vL (if (keyword? v) vL v)))

(defn call-run* [r a b c]
  "run* wrapper"
  (run* [q]
    (fresh [a-l b-l c-l]
      (to-lvar a a-l)
      (to-lvar b b-l)
      (to-lvar c c-l)
      (r a-l b-l c-l)
      (== q (list a-l b-l c-l)))))
;; --------------------------------------
;; (relational)

(defn add-peano-r [x y s]
  "sum of 2 peano numbers"
  (call-run* appendo x y s))

(defn add-r [x y s]
  (peano->n (add-peano-r (n->peano x) (n->peano y) (n->peano s))))

(defn sub-peano-r [x y d]
  "difference between 2 peano numbers"
  (let [r (add-peano-r d y x)]
    (loop [l r accu ()]
      (if (empty? l) accu
          (recur (rest l) (cons (let [[d y x] (first l)] `(~x ~y ~d)) accu))))))

(defn sub-r [x y d]
  (peano->n (sub-peano-r (n->peano x) (n->peano y) (n->peano d))))


(defn multo-v1 [x y p]      ; x * y = p
  "broken: does not terminate for the following argument sets: (:x () (\S ...)) (:x :y (\S ...))"
  (conde
   [(== () x) (== () p)]
   [(fresh [a b c]
      (== (lcons a b) x)    ; b = x - 1 (also implies x != ())
      (appendo y c p)       ; y + c = p   ==   y + (x - 1) * y = p
      (multo-v1 b y c))]))  ; (x - 1) * y = c

(defn multo-v2 [x y p]      ; x * y = p
  "semi-broken: terminate for the following argument sets: (:x () (\S ...)) (:x :y (\S ...))
   but results look like:
   (mult-r :x :y 3) ; (((_0 _1 _2) (\S) (\S \S \S)) ((_0) (\S \S \S) (\S \S \S)))
   reason: head elements (i.e., lvars a & d below) are/remain lvars"
  (conde
   [(== () x) (== () p)]
   [(== () y) (== () p)]
   [(fresh [xh xt yh yt c]
      (== (lcons xh xt) x)  ; xt = x - 1 (also implies x != ())
      (== (lcons yh yt) y)  ; (also implies y != ())
      (appendo y c p)       ; y + c = p   ==   y + (x - 1) * y = p
      (multo-v2 xt y c))])) ; (x - 1) * y = c

(defn multo-v3 [x y p]      ; x * y = p
  "fix for introduction of lvars as head elements
   but results can look like:
  (peano->n (mult-r 0 :y 0)) ; ((0 :any 0) (0 0 0))
  reason: TODO"
  (conde
   [(== () x) (== () p)]
   [(== () y) (== () p)]
   [(fresh [xt yt c]
      (== (lcons \S xt) x)  ; (if x is an lvar, head is unified with a proper \S)
      (== (lcons \S yt) y)  ; (if y is an lvar, head is unified with a proper \S)
      (appendo y c p)
      (multo-v3 xt y c))]))

(defn mult-peano-r [x y p]
  "product of 2 peano numbers"
  (call-run* multo-v3 x y p))

(defn mult-r [x y p]
;;  (peano->n (mult-peano-r (n->peano x) (n->peano y) (n->peano p)))
  (mult-peano-r (n->peano x) (n->peano y) (n->peano p))
  )


(defn div-r [x y q]
  "quotient of 2 peano numbers"
  )

(defn divo [x y q]
  (peano->n (mult-peano-r (n->peano x) (n->peano y) (n->peano q))))

;; examples:
;; (from-peano (addo-peano (to-peano 1) (to-peano 2) :s))  ; ((1 2 3))
;; (from-peano (addo-peano :x :y (to-peano 3)))            ; ((0 3 3) (1 2 3) (2 1 3) (3 0 3))

;; (from-peano (subo-peano (to-peano 3) (to-peano 1) :d))  ; ((3 1 2))
;; (from-peano (subo-peano (to-peano 3) :a :b))            ; ((3 0 3) (3 1 2) (3 2 1) (3 3 0))
;; --------------------------------------
;;
