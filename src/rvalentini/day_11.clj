(ns rvalentini.day_11
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn within-bounds?
  [[n m]]
  (and (<= 0 n 9) (<= 0 m 9)))

(defn get-neighbors
  [[n m]]
  (filter within-bounds? [[(dec n) m] [(inc n) m] [n (dec m)] [n (inc m)]
                          [(dec n) (dec m)] [(inc n) (dec m)] [(dec n) (inc m)] [(inc n) (inc m)]]))

(defn map-at-pos
  [fn grid [n m]]
  (if (and n m)
    (let [[n1 n2] (split-at n grid)
          [m1 m2] (split-at m (first n2))]
      (concat n1
        (list (concat m1 (list (fn (first m2))) (drop 1 m2)))
        (drop 1 n2)))
    grid))

(def inc-at-pos (partial map-at-pos inc))

(defn inc-all [grid] (reduce inc-at-pos grid (for [x (range 10) y (range 10)] [x y])))

(def zero-at-pos (partial map-at-pos (constantly 0)))

(defn get-flash-ready-octos
  [grid]
  (filter some? (for [[x col] (map-indexed vector grid)
                      [y energy] (map-indexed vector col)]
                  (when (> energy 9) [x y]))))

(defn flash
  [grid pos]
  (let [neighbors (get-neighbors pos)]
    (reduce inc-at-pos grid neighbors)))

(defn step
  [[grid _]]
  (loop [grid (inc-all grid)
         flashed #{}]
    (let [about-to-flash (filter #(not (contains? flashed %)) (get-flash-ready-octos grid))]
      (if (empty? about-to-flash)
        [(reduce zero-at-pos grid flashed) (count flashed)]
        (recur
          (reduce flash grid about-to-flash)
          (into flashed about-to-flash))))))

(comment
  (with-open [rdr (io/reader (io/resource "day_11_input.txt"))]
    (let [grid (->> (line-seq rdr)
                 (map #(s/split % #""))
                 (map #(map parse-long %)))]

      (println "Sum of flashes after 100 steps:"
        (->> (iterate step [grid 0])
          (take 101)
          (reduce #(+ % (second %2)) 0)))

      (println "First step that causes all octos to flash: "
        (->> (iterate step [grid 0])
          (map second)
          (map-indexed vector)
          (take-while #(not= 100 (second %)))
          last
          first
          inc))))
  )
