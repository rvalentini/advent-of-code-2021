(ns rvalentini.day_9
  (:require [clojure.java.io :as io]))

(defn within-bounds?
  [[n m]]
  (and (<= 0 n 99) (<= 0 m 99)))

(defn get-neighbor-points
  [n m]
  (filter within-bounds? [[(dec n) m] [(inc n) m] [n (dec m)] [n (inc m)]]))

(defn get-height
  [heightmap [n m]]
  (Integer/parseInt (str (nth (nth heightmap n) m))))

(defn get-basin-points
  [heightmap basin [n m]]
  (->> (get-neighbor-points n m)
    (filter #(not (contains? basin %)))
    (filter #(not= (get-height heightmap %) 9))))

(defn expand-basin
  [height-map start]
  (loop [[first-candidate & rest] (list start)
         basin #{}]
    (if (nil? first-candidate)
      basin
      (let [rest-expanded (into rest (get-basin-points height-map basin first-candidate))]
        (recur rest-expanded (conj basin first-candidate))))))

(defn is-low-point?
  [heightmap [n m]]
  (let [height (get-height heightmap [n m])
        s-points-heights (map #(get-height heightmap %) (get-neighbor-points n m))]
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

      (println "Multiplied sizes of three largest basins:"
        (apply * (->> (for [n (range 100) m (range 100)] [n m])
                   (filter #(not= (get-height height-map %) 9))
                   (map #(expand-basin height-map %))
                   (map count)
                   set
                   sort
                   reverse
                   (take 3))))))
  )



