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

(comment
  (with-open [rdr (io/reader (io/resource "day_2_input.txt"))]
    (let [[x y] (reduce sum-vecs [0 0]
                  (->> (line-seq rdr)
                    (map #(str/split % #" "))
                    (map to-vector)))]
      (* x y)))

  )



