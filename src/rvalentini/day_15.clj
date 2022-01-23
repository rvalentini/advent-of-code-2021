(ns rvalentini.day_15
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn within-bounds?
  [[n m]]
  (and (<= 0 n 99) (<= 0 m 99)))

(defn get-neighbor-coordinates
  [n m]
  (filter within-bounds? [[(dec n) m] [(inc n) m] [n (dec m)] [n (inc m)]]))

(defn not-visited? [[_ props]] (not (props :visited)))

(defn update-dist [old dist] (assoc old :dist dist))

(defn update-prev [old prev] (assoc old :prev prev))

(defn mark-as-visited [old] (assoc old :visited true))

(defn get-neighbors
  [cave-map [n m]]
  (filter not-visited?
    (for [c (get-neighbor-coordinates n m)]
      [c (cave-map c)])))

(defn build-cave-map
  [input]
  (into {} (for [[x row] (map-indexed vector input)
                 [y val] (map-indexed vector row)]
             [[x y] {:dist ##Inf :prev nil :risk (parse-long val)}])))

(defn get-safest-next-pos
  [cave-map]
  (apply min-key #((second %) :dist) (filter not-visited? (seq cave-map))))

(defn update-neighbor
  [cave-map [pos props] [n-pos n-props]]
  (let [new-dist (+ (props :dist) (n-props :risk))]
    (if (< new-dist (n-props :dist))
      (-> cave-map
        (update n-pos #(update-dist % new-dist))
        (update n-pos #(update-prev % pos)))
      cave-map)))

(defn update-all-neighbors
  [cave-map current neighbors]
  (reduce #(update-neighbor %1 current %2) cave-map neighbors))

(defn find-safest-path
  [cave-map start-pos end-pos]
  (loop [cave-map (update cave-map start-pos #(update-dist % 0))]
    (let [current (get-safest-next-pos cave-map)]
      (if (= (first current) end-pos)
        current
        (let [neighbors (get-neighbors cave-map (first current))]
          (recur (-> cave-map
                   (update (first current) mark-as-visited)
                   (update-all-neighbors current neighbors))))))))

(comment
  (with-open [rdr (io/reader (io/resource "day_15_input.txt"))]
    (let [input (->> (line-seq rdr)
                  (map #(s/split % #"")))
          goal (find-safest-path (build-cave-map input) [0 0] [99 99])]
      (println "Lowest total risk from start to end: " ((second goal) :dist))))

  )

