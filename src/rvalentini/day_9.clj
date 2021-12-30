(ns rvalentini.day_9
  (:require [clojure.java.io :as io]))

(defn within-bounds?
  [[n m]]
  (and (<= 0 n 99) (<= 0 m 99)))

(defn get-surrounding-points
  [n m]
  (filter within-bounds? [[(dec n) m] [(inc n) m] [n (dec m)] [n (inc m)]]))

(defn get-height
  [heightmap [n m]]
  (Integer/parseInt (str (nth (nth heightmap n) m))))

(defn is-low-point?
  [heightmap [n m]]
  (let [height (get-height heightmap [n m])
        s-points-heights (map #(get-height heightmap %) (get-surrounding-points n m))]
    (every? #(> % height) s-points-heights)))

(comment
  (with-open [rdr (io/reader (io/resource "day_9_input.txt"))]
    (let [height-map (->> (line-seq rdr)
                       (map (comp vec seq)))]
      (println "Risk level of low points:"
        (apply + (->> (for [n (range 100) m (range 100)] [n m])
                   (filter #(is-low-point? height-map %))
                   (map #(get-height height-map %))
                   (map inc))))

      ))

  )



