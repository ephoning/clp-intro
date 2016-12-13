(ns clp-intro.peano-test
  (:require [clojure.test :refer :all]
            [clp-intro.peano :refer :all]))

;; run tests with 'lein test clp-intro.peano-test'

(deftest ^:unit mult
  (is (= '((_0  0  0))  (mult-r :x  0  0)))
  (is (= '(( 0 42  0))  (mult-r  0 42 :p)))
  (is (= '((_0  0  0))  (mult-r :x  0 :p)))
  (is (= '((42  0  0))  (mult-r 42  0 :p)))
  (is (= '(( 0 _0  0) (_0  0  0))  (mult-r :x :y  0)))
  (is (= '((42  0  0))  (mult-r 42 :y 0)))
  (is (= '(( 0 42  0))  (mult-r :x 42  0)))
  (is (= '(( 0  0  0))  (mult-r  0  0 :p)))
  (is (= '(( 0 _0  0))  (mult-r  0 :y  0)))
  (is (= '(( 0 42  0))  (mult-r  0 42  0)))
  (is (= '((42  0  0))  (mult-r 42  0  0)))
  (is (= '(( 0  0  0))  (mult-r  0  0  0)))

  (is (= 0              (mult-r :x  0 42)))
  (is (= 0              (mult-r  0 :y 42)))

  (is (= '(( 0 _0  0))  (mult-r  0 :y :p)))

  (is (= '(( 6  7 42))  (mult-r :x  7 42)))
  (is (= '(( 6  7 42))  (mult-r  6 :y 42)))
  (is (= '(( 6  7 42))  (mult-r  6  7 :p)))

  (is (= '((4 1 4) (2 2 4) (1 4 4))  (mult-r :x :y  4)))
  )
