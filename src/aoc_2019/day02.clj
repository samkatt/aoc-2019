(ns aoc-2019.day02
  (:require [clojure.java.io :as io]))

(def demo-str "1,9,10,3,2,3,11,0,99,30,40,50")

(defn parse-input
  [str-descr]
  (vec (map #(Long/parseLong %) (re-seq #"\d+" str-descr))))

(def demo-input (parse-input demo-str))
; (demo-input) ; [1 9 10 3 2 3 11 0 99 30 40 50]

(defn step-program [pointer mem]
  (let [[opt in1 in2 out] (subvec mem pointer (+ pointer 4))]
    (cond
      (= opt 1) (assoc mem out (+ (mem in1) (mem in2)))
      (= opt 2) (assoc mem out (* (mem in1) (mem in2)))
      :else (throw (new Exception (str "got opt " opt))))))

(step-program 0 demo-input) ; [1 9 10 70 2 3 11 0 99 30 40 50]

(defn apply-opts [pointer mem]
  (if (= 99 (mem pointer))
    mem
    (recur (+ pointer 4) (step-program pointer mem))))

; some testing
(apply-opts 0 demo-input) ; [3500 9 10 70 2 3 11 0 99 30 40 50]

(apply-opts 0 (parse-input "1,0,0,0,99")) ; [2 0 0 0 99]
(apply-opts 0 (parse-input "2,3,0,3,99")) ; [2 3 0 6 99]
(apply-opts 0 (parse-input "2,4,4,5,99,0")) ; [2 4 4 5 99 9801]
(apply-opts 0 (parse-input "1,1,1,4,99,5,6,0,99")) ; [30 1 1 4 2 5 6 0 99]

(defn update-program-noun-and-verb
  [noun verb input] (assoc input 1 noun 2 verb))

(def input (parse-input (slurp (io/resource "input_02.txt"))))

(defn run-program-with-updated [noun verb input]
  (apply-opts 0 (update-program-noun-and-verb noun verb input)))

(first (run-program-with-updated 12 2 input)) ; 2842648

; part 2
; try different initial memories 1 and 2 to produce 19690720
(defn program-sequence
  [input]
  (for [noun (range 99) verb (range 99)]
    (run-program-with-updated noun verb input)))

(def res-2 (let [output (some #(when (= (% 0) 19690720) %) (program-sequence input))]
             (+ (* 100 (output 1)) (output 2)))) ; 9074
(print res-2)
