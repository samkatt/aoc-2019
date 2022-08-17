(ns aoc-2019.day01
  (:require [clojure.java.io :as io]))

; 12 -> 2
; 14 -> 2
; 1969 -> 654
; 100756 -> 33583

(def demo-input [12 14 1969 100756])
(defn mass->gas
  [mass]
  (max 0 (- (int (/ mass 3)) 2)))

(mass->gas 1969) ; 654
(apply + (map mass->gas demo-input)) ; 34241

(def input
  (map #(Long/parseLong %)
       (line-seq (io/reader (io/resource "input_01.txt")))))

(apply + (map mass->gas input)) ; 3336439

(defn recur-mass->fuel
  [mass]
  (if (= mass 0)
    0
    (let [fuel (mass->gas mass)]
      (+ fuel (recur-mass->fuel fuel)))))

(recur-mass->fuel 1969) ; 966

(def res-2 (apply + (map recur-mass->fuel input))) ; 5001791

(print res-2)
