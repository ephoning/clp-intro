(ns clp-intro.peano-v2
  (:require [clojure.core.logic :refer :all]
            [clp-intro.basics :refer :all]))

;; --------------------------------------
;; CLP
;; V2
;; - no longer necessarily expect only ONE answer (although we can get only one here...)
;;
(defn addo-peano-V2 [x y s]
  (cond
    (keyword? x) (let [yP (to-peano y)
                       sP (to-peano s)
                       xP (run* [q] (appendo q yP sP))]
                   (apply from-peano-V2 xP))
    (keyword? y) (let [xP (to-peano x)
                       sP (to-peano s)
                       yP (run* [q] (appendo xP q sP))]
                   (apply from-peano-V2 yP))
    (keyword? s) (let [xP (to-peano x)
                       yP (to-peano y)
                       sP (run* [q] (appendo xP yP q))]
                   (apply from-peano-V2 sP))))
