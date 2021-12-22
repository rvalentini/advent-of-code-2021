(ns rvalentini.day_5
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn str->line
  [str]
  (->> str
    (map #(s/split % #","))
    (map (fn [t] (map #(Integer/parseInt %) t)))))

(defn inclusive-range
  [a b]
  (cond
    (= a b) (repeat a)
    (< a b) (range a (inc b))
    :else (range b (inc a))))

(defn line->coordinates
  [[[x1 y1] [x2 y2]]]
  (let [range-x (inclusive-range x1 x2)
        range-y (inclusive-range y1 y2)]
    (map vector
      (if (< x1 x2) (reverse range-x) range-x)
      (if (< y1 y2) (reverse range-y) range-y))))

(defn frequency-reducer
  [freq-map coordinate]
  (assoc freq-map coordinate (if-let [curr (get freq-map coordinate)] (inc curr) 1)))

(comment
  (with-open [rdr (io/reader (io/resource "day_5_input.txt"))]
    (let [lines  (->> (line-seq rdr)
                   (map #(s/split % #"\s->\s"))
                   (map str->line))
          coordinates (->> lines
                        (map line->coordinates)
                        (apply concat)
                        (filter some?))]
      (println "Number of overlapping points: "
        (count (filter #(> (val %) 1) (reduce frequency-reducer {} coordinates))))))
  )



