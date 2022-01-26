(ns rvalentini.day_15
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn within-bounds?
  [[n m]]
  (and (<= 0 n 499) (<= 0 m 499)))

(defn get-neighbor-coordinates
  [n m]
  (filter within-bounds? [[(dec n) m] [(inc n) m] [n (dec m)] [n (inc m)]]))

(defn not-visited? [[_ props]] (not (props :visited)))

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
        (update n-pos #(assoc % :dist new-dist))
        (update n-pos #(assoc % :prev pos)))
      cave-map)))

(defn update-all-neighbors
  [cave-map current neighbors]
  (reduce #(update-neighbor %1 current %2) cave-map neighbors))

(defn find-safest-path
  [cave-map start-pos end-pos]
  (loop [cave-map (update cave-map start-pos #(assoc % :dist 0))]
    (let [current (get-safest-next-pos cave-map)]
      (if (= (first current) end-pos)
        current
        (let [neighbors (get-neighbors cave-map (first current))]
          (recur (-> cave-map
                   (update (first current) mark-as-visited)
                   (update-all-neighbors current neighbors))))))))

(defn inc-risk [r] (if (> r 9) (- r 9) r))

(defn expand-map
  [cave-map]
  (apply merge
    (for [x-factor (range 0 5)
          y-factor (range 0 5)]
      (into {} (for [[k v] cave-map]
                 [[(+ (first k) (* x-factor 100)) (+ (second k) (* y-factor 100))]
                  (update v :risk #(inc-risk (+ % x-factor y-factor)))])))))

(comment
  (with-open [rdr (io/reader (io/resource "day_15_input.txt"))]
    (let [input (->> (line-seq rdr)
                  (map #(s/split % #"")))
          result (find-safest-path (expand-map (build-cave-map input)) [0 0] [499 499])]
      (println "Lowest total risk from start to end: " ((second result) :dist))))
  )
