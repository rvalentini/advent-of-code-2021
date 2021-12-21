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
  (if (< a b)
    (range a (inc b))
    (range b (inc a))))

(defn interpolate
  [a b c]
  (map vector (inclusive-range a b) (repeat c)))

(defn line->coordinates
  [[start end]]
  (cond
    (= (first start) (first end)) (map reverse (interpolate (second start) (second end) (first start)))
    (= (second start) (second end)) (interpolate (first start) (first end) (second start))
    :else nil))

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



