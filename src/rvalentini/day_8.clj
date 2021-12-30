(ns rvalentini.day_8
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(defn solve-set-5
  [solution set-5]
  (if (set/subset? (:1 solution) set-5)
    (assoc solution :3 set-5)
    (if (= (count (set/intersection (:4 solution) set-5)) 2)
      (assoc solution :2 set-5)
      (assoc solution :5 set-5))))

(defn solve-set-6
  [solution set-6]
  (if (= (count (set/intersection (:4 solution) set-6)) 4)
    (assoc solution :9 set-6)
    (if (= (count (set/intersection (:7 solution) set-6)) 3)
      (assoc solution :0 set-6)
      (assoc solution :6 set-6))))

(defn solve
  [solution curr]
  (case (count curr)
    2 (assoc solution :1 curr)
    3 (assoc solution :7 curr)
    4 (assoc solution :4 curr)
    5 (solve-set-5 solution curr)
    6 (solve-set-6 solution curr)
    7 (assoc solution :8 curr)
    :default (throw (IllegalArgumentException.
                      (str "Unknown digit definition: " curr)))))

(defn decrypt-output
  [lines]
  (for [line lines]
    (let [definitions (take 10 line)
          display (take-last 4 line)
          solution (reduce solve {} (sort-by count definitions))]
      (flatten (map #(filter (comp #{%} solution) (keys solution)) display)))))

(defn output->number
  [output]
  (->> output
    (map #(map name %))
    (map #(apply str %))
    (map #(Integer/parseInt %))))

(comment
  (with-open [rdr (io/reader (io/resource "day_8_input.txt"))]
    (let [lines (->> (line-seq rdr)
                      (map #(s/split % #"\s+"))
                      (map #(into (subvec % 0 10) (subvec % 11)))
                      (map #(->> %
                              (map (fn [digit] (s/split digit #"")))
                              (map set))))
          output-vals (decrypt-output lines)]

      (println "Overall count of 1,4,7,8s: " (count (filter #(#{:1 :4 :7 :8} %) (flatten output-vals))))
      (println "Sum of all output values: " (reduce + (output->number output-vals)))))
  )



