(ns rvalentini.day_2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn sum-vecs
  [[x-sum y-sum] [x y]]
  [(+ x-sum x) (+ y-sum y)])

(defn to-vector
  [[direction value]]
  (let [magnitude (Integer/parseInt value)]
    (case direction
      "down" [0 magnitude]
      "up" [0 (- magnitude)]
      "forward" [magnitude 0])))

(defn aggregate-with-aim
  [[x y aim] [direction value]]
  (let [magnitude (Integer/parseInt value)]
    (case direction
      "down" [x y (+ aim magnitude)]
      "up" [x y (- aim magnitude)]
      "forward" [(+ x magnitude) (+ y (* aim magnitude)) aim])))

(comment
  (with-open [rdr (io/reader (io/resource "day_2_input.txt"))]
    (let [[x y] (reduce sum-vecs [0 0]
                  (->> (line-seq rdr)
                    (map #(str/split % #" "))
                    (map to-vector)))]
      (* x y)))

  (with-open [rdr (io/reader (io/resource "day_2_input.txt"))]
    (let [[x y] (reduce aggregate-with-aim [0 0 0]
                  (->> (line-seq rdr)
                    (map #(str/split % #" "))))]
      (* x y)))
  )



