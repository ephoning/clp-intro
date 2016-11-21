(ns clp-intro.peano-v2
  (:require [clojure.core.logic :refer :all]
            [clp-intro.basics :refer :all]))

;; --------------------------------------
;; - no longer necessarily expect only ONE answer (although we can get only one here...)
;;
(defn addo-peano [x y s]
  (cond
    (keyword? x) (let [yP (to-peano y)
                       sP (to-peano s)
                       xP (run* [q] (appendo q yP sP))]
                   (apply from-peano-flat xP))
    (keyword? y) (let [xP (to-peano x)
                       sP (to-peano s)
                       yP (run* [q] (appendo xP q sP))]
                   (apply from-peano-flat yP))
    (keyword? s) (let [xP (to-peano x)
                       yP (to-peano y)
                       sP (run* [q] (appendo xP yP q))]
                   (apply from-peano-flat sP))))
