(ns clp-intro.peano
  (:require [clojure.core.logic :refer :all]
            [clojure.string :as str]
            :reload))


;; convert natural number to Peano representation
;; (if given a keyword, leave it untouched)
(defn n->peano
  "convert natural number to peano representation (but leave keywords as-is)"
  [n]
  (if (keyword? n) n (if (= n 0) () (cons \S (n->peano (dec n))))))

(defn peano? [p]
  (and (seq? p) (or (empty? p) (= (first p) \S))))

(defn peano->n
  "convert potentially nested peano representation(s) to natural number(s)"
  [ps]
  (cond (peano? ps) (count ps)
        (symbol? ps) ps ; ('_0' or '_1', etc.)
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
(defn to-lvar
  "unify lvar with itself (no-op) or passed-in value
  (this, in effect, 'promotes' all arguments to lvars; keywords to fresh lvars)"
  [v vL]
  (== vL (if (keyword? v) vL v)))

(defn call-relation [r a b c q]
  (fresh [a-l b-l c-l]
    (to-lvar a a-l)
    (to-lvar b b-l)
    (to-lvar c c-l)
    (r a-l b-l c-l)
    (== q (list a-l b-l c-l))))

(defn call-run
  "run* / run n wrapper"
  ([r a b c]
   (run* [q] (call-relation r a b c q)))
  ([r n a b c]
   (run n [q] (call-relation r a b c q))))

;; --------------------------------------
;; (relational)

(defn add-peano-r
  "x + y = s"
  [x y s]
  (call-run appendo x y s))

(defn add-r [x y s]
  (peano->n (add-peano-r (n->peano x) (n->peano y) (n->peano s))))

(defn sub-peano-r
  "x - y = d"
  [x y d]
  (let [r (add-peano-r d y x)]
    (loop [l r accu ()]
      (if (empty? l) accu
          (recur (rest l) (cons (let [[d y x] (first l)] `(~x ~y ~d)) accu))))))

(defn sub-r [x y d]
  (peano->n (sub-peano-r (n->peano x) (n->peano y) (n->peano d))))


(defn multo-v1
  "x * y = p
   broken: does not terminate for the following argument sets: (:x () (S ...)) (:x :y (S ...))"
  [x y p]
  (conde
   [(== () x) (== () p)]
   [(fresh [a b c]
      (== (lcons a b) x)    ; b = x - 1 (also implies x != ())
      (appendo y c p)       ; y + c = p   ==   y + (x - 1) * y = p
      (multo-v1 b y c))]))  ; (x - 1) * y = c

(defn multo-v2
  "x * y = p
   semi-broken; now terminats for the following argument sets: (:x () (S ...)) (:x :y (S ...))
   but results (in peano notation) look like:
   (mult-r :x :y 3) ; (((_0 _1 _2) (S) (S S S)) ((_0) (S S S) (S S S)))
   reason: head elements (i.e., lvars a & d below) are/remain lvars when 'x' and 'y' are constructed
   versus destructured"
  [x y p]
  (conde
   [(== () x) (== () p)]
   [(== () y) (== () p)]
   [(fresh [xh xt yh yt c]
      (== (lcons xh xt) x)  ; xt = x - 1 (also implies x != ())
      (== (lcons yh yt) y)  ; (also implies y != ())
      (appendo y c p)       ; y + c = p   ==   y + (x - 1) * y = p
      (multo-v2 xt y c))])) ; (x - 1) * y = c

(defn multo-v3
  "x * y = p
   fix for introduction of lvars as head elements: unify head elements with S
   but results can be:
   (mult-r 0 :y 0) ; ((0 _0 0) (0 0 0))    (both the 'general' and a 'specific' solution are present)
   reason: input matches multiple conde clauses"
  [x y p]
  (conde
   [(== () x) (== () p)] ; this clause results in (mult-r :x 0 0) giving (0 0 0)
   [(== () y) (== () p)] ; this clause results in (mult-r :x 0 0) giving (_0 0 0)
   [(fresh [xt yt c]
      (== (lcons \S xt) x)
      (== (lcons \S yt) y)
      (appendo y c p)
      (multo-v3 xt y c))]))

(defn multo-v4
  "x * y = p
   fix for generating both the generic (univ quantification) and specific case when dealing with '0's
   but the following results in 'no solution':
   (mult-r :x 23 0) ; 0"
  [x y p]
  (conde
   [(== () x) (lvaro y) (== () p)] ; adding 'lvaro' constraint avoids: (mult-r :x 0 0) ; (0 0 0)
   [(lvaro x) (== () y) (== () p)] ; adding 'lvaro' constraint avoids: (mult-r 0 :y 0) ; (0 0 0)
   [(fresh [xt yt c]
      (== (lcons \S xt) x)
      (== (lcons \S yt) y)
      (appendo y c p)
      (multo-v3 xt y c))]))

(defn zero-n [v] (all (nonlvaro v) (== () v)))
(defn nonzero-n [v] (fresh[t] (nonlvaro v) (== (lcons \S t) v)))
(defn nonzero-ln [v] (conde [(nonzero-n v)] [(lvaro v)]))

(defn multo-v5
  "x * y = p
   fix for ALL argument combinations containing at least one 0
   Q: can we avoid having to spec out ALL valid 0/n/lvar permutations (see below) across x y p?"
  [x y p]
  (conde
   [(zero-n x) (lvaro y)     (lvaro p)  (== () p)]   ;  0 :y :p  <all>-0
   [(zero-n x) (nonzero-n y) (lvaro p)  (== () p)]   ;  0  n :p  0
   ; 0 :y  n  discard: no solution
   ; 0  n  n  discard: no solution
   [(lvaro x) (zero-n y) (lvaro p)      (== () p)]   ; :x  0 :p  <all>-0
   ; :x 0  n  discard: no solution
   [(nonzero-n x) (zero-n y)(lvaro p)   (== () p)]   ;  n  0 :p  0
   ;  n 0  n  discard: no solution
   [(lvaro x) (lvaro y) (zero-n p)      (== () x)]   ; :x :y  0  <all>-0 / 0-<all>
   [(lvaro x) (lvaro y) (zero-n p)      (== () y)]   ; :x :y  0  <all>-0 / 0-<all>
   [(nonzero-n x) (lvaro y) (zero-n p)  (== () y)]   ;  n :y  0  0
   [(lvaro x) (nonzero-n y) (zero-n p)  (== () x)]   ; :x  n  0  0
   ;  n  n 0  discard: no solution
   [(zero-n x) (zero-n y) (lvaro p)     (== () p)]   ;  0  0 :p  0
   ;  0 0  n  discard: no solution
   [(zero-n x) (lvaro y) (zero-n p)]                 ;  0 :y  0  <all>
   [(zero-n x) (nonzero-n y) (zero-n p)]             ;  0  n  0
   [(lvaro x)  (zero-n y) (zero-n p)]                ; :x  0  0  <all>
   [(nonzero-n x) (zero-n y) (zero-n p)]             ;  n  0  0
   [(zero-n x) (zero-n y) (zero-n p)]                ;  0  0  0

   [(fresh [xt yt c]
      (== (lcons \S xt) x)
      (== (lcons \S yt) y)
      (appendo y c p)
      (multo-v3 xt y c))]))

(defn multo-v6
  "x * y = p
   replace multiple combinations that can be subsumed by a single alternative
   Q: is there a better/proper way to handle this / avoid the comb explosion of clauses?
      (multiplication appears to be more complex than addition because of '0')"
  [x y p]
  (conde
   [(zero-n x) (lvaro y)     (lvaro p)  (== () p)]   ;  0 :y :p  <all>-0
   [(zero-n x) (nonzero-n y) (lvaro p)  (== () p)]   ;  0  n :p  0

   [(lvaro x) (zero-n y) (lvaro p)      (== () p)]   ; :x  0 :p  <all>-0
   [(nonzero-n x) (zero-n y)(lvaro p)   (== () p)]   ;  n  0 :p  0

   [(lvaro x) (lvaro y) (zero-n p)      (== () x)]   ; :x :y  0  <all>-0 / 0-<all>
   [(lvaro x) (lvaro y) (zero-n p)      (== () y)]   ; :x :y  0  <all>-0 / 0-<all>
   [(nonzero-n x) (lvaro y) (zero-n p)  (== () y)]   ;  n :y  0  0
   [(lvaro x) (nonzero-n y) (zero-n p)  (== () x)]   ; :x  n  0  0

   [(zero-n x) (zero-n y) (lvaro p)     (== () p)]   ;  0  0 :p  0

   ;; [(zero-n x) (lvaro y) (zero-n p)]                 ;  0 :y  0  <all>
   ;; [(zero-n x) (nonzero-n y) (zero-n p)]             ;  0  n  0
   [(zero-n x) (nonzero-ln y) (zero-n p)]               ;  0  _  0

   ;; [(nonzero-n x) (zero-n y) (zero-n p)]             ;  n  0  0
   ;; [(lvaro x)  (zero-n y) (zero-n p)]                ; :x  0  0  <all>
   ;; [(zero-n x) (zero-n y) (zero-n p)]                ;  0  0  0
   [(zero-n y) (zero-n p)]                              ;  _  0  0

   [(fresh [xt yt c]
      (== (lcons \S xt) x)
      (== (lcons \S yt) y)
      (appendo y c p)
      (multo-v3 xt y c))]))

(def multo multo-v6)

(defn mult-peano-r
  "product of 2 peano numbers"
  ([x y p] (call-run multo x y p))
  ([x y p n] (call-run multo n x y p)))

(defn mult-r
  "x * y = p"
  ([x y p] (peano->n (mult-peano-r (n->peano x) (n->peano y) (n->peano p))))
  ([x y p n] (peano->n (mult-peano-r (n->peano x) (n->peano y) (n->peano p) n))))

(defn div-peano-r
  "x / y = q"
  [x y q]
  (let [r (mult-peano-r q y x)]
    (loop [l r accu ()]
      (if (empty? l) accu
          (recur (rest l) (cons (let [[q y x] (first l)] `(~x ~y ~q)) accu))))))

(defn div-r
  "x / y = q"
  [x y q]
  (peano->n (div-peano-r (n->peano x) (n->peano y) (n->peano q))))

;; examples:
;; (mult-r 6 7 :p)   ; ((6 7 42))
;; (mult-r :x :y 6)  ; ((6 1 6) (3 2 6) (2 3 6) (1 6 6))
;; (mult-r :x 0 0)   ; ((_0 0 0))
;; (mult-r 0 :y 0 4) ; ((0 _0 0))
;;
;; (div-r 42 6 :q)   ; ((42 6 7))
;; (div-r 42 :y :q)  ; ((42 42 1) (42 21 2) (42 14 3) (42 7 6) (42 6 7) (42 3 14) (42 2 21) (42 1 42))
;; (div-r 42 0 :q)   ; 0
