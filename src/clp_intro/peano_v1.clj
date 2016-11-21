(ns clp-intro.peano-v1
  (:require [clojure.core.logic :refer :all]
            [clp-intro.basics :refer :all]))

;; --------------------------------------
;; - cannot deal with multiple LVars, only one...
;; - have to extract the 'first' element from the result of 'run*'
;;
(defn addo-peano [x y s]
  (cond
    (keyword? x) (let [yP (to-peano y)
                       sP (to-peano s)
                       xP (first (run* [q] (appendo q yP sP)))]
                   (from-peano-single xP))
    (keyword? y) (let [xP (to-peano x)
                       sP (to-peano s)
                       yP (first (run* [q] (appendo xP q sP)))]
                   (from-peano-single yP))
    (keyword? s) (let [xP (to-peano x)
                       yP (to-peano y)
                       sP (first (run* [q] (appendo xP yP q)))]
                   (from-peano-single sP))))
