(ns aoc-2019.day03
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.set :as set]))

; helper functions
(defn abs
  [x]
  (if (< x 0) (- x) x))

(abs -1) ; 1
(abs 1) ; 1

(defn manh
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(manh [0 0] [1 1]) ; 2
(manh [0 0] [-1 1]) ; 2
(manh [-3 2] [10 3]) ; 14

(def demo-input-1 "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83")
(def demo-input-2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

; general idea is:
;   - create the (relative) path of both cables
;   - store them as sets
;   - find the intersection
;   - compute Manhattan for each intersection
;   - return smallest Manhattan

(defn str->opts
  "Transforms a string description of the operations into [['R' 10]] data structure"
  [str-descr]
  (map #(vector (% 1) (Long/parseLong (% 2))) (re-seq #"(.)(\d+)" str-descr)))

(def test-opts (str->opts (first (s/split-lines demo-input-1))))
(first test-opts) ; ["R" 75]

(defn flatten-operations
  "Creates a sequence from 'big operations' ['R' 3] to multiple small (3) ones ['R'...]] "
  [opts]
  (mapcat #(repeat (% 1) (% 0)) opts))

(take 10 (flatten-operations test-opts)) ; ("R" "R" "R" "R" "R" "R" "R" "R" "R" "R")

(defn step
  "Takes direction current position and return next position"
  [dir [x y]]
  (cond
    (= dir "R") [(dec x) y]
    (= dir "L") [(inc x) y]
    (= dir "D") [x (dec y)]
    (= dir "U") [x (inc y)]))

(step "R" [0 1]) ; [-1 1]
(step "D" [-10 1]) ; [-10 0]
(step "L" [-1 100]) ; [0 100]

(defn opts->path
  "Gets operations and returns a sequence of positions (path)"
  [opts]
  (loop
   [path [[0 0]] [opt & opts] (flatten-operations opts)]
    (if (nil? opt) path
        (recur (conj path (step opt (peek path))) opts))))

(defn str->path
  "So because I do not know how to do nested function calls properly, I'm just making them explicitly"
  [str-descr]
  (opts->path (str->opts str-descr)))

(defn paths->intersection
  "Get the intersections of two paths"
  [p1 p2]
  (set/intersection (set p1) (set p2)))

(defn part-1
  "returns smallest Manhattan distance of two paths"
  [str-descr]
  (second (sort (map #(manh [0 0] %) (apply paths->intersection (map str->path (s/split-lines str-descr)))))))

(part-1 demo-input-1) ; 159
(part-1 demo-input-2) ; 135

(part-1 (slurp (io/resource "input_03.txt"))) ; 293

; part 2: minimize by path length
(defn pos->summed-distance
  "Computes the distance of `pos` in `path`"
  [pos p1 p2]
  (+ (.indexOf p1 pos) (.indexOf p2 pos)))

(defn part-2
  "returns smallest path distance of intersections of two paths"
  [str-descr]
  (let [[p1 p2] (map str->path (s/split-lines str-descr))
        intersections (paths->intersection p1 p2)]
    (second (sort (map #(pos->summed-distance % p1 p2) intersections)))))

(part-2 demo-input-1) ; 610
(part-2 demo-input-2) ; 410

(def res-2 (part-2 (slurp (io/resource "input_03.txt")))) ; 27306
(print res-2)
