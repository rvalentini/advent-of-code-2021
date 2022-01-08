(ns rvalentini.day_13
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn fold-point
  [[axis line] [x y]]
  (case axis
    :x (if (> x line) [(- line (- x line)) y] [x y])
    :y (if (> y line) [x (- line (- y line))] [x y])))

(defn str->pos [str] (map parse-long (s/split str #",")))

(defn str->fold
  [st]
  (let [[f1 f2] (s/split st #"=")]
    [(keyword (str (last f1))) (parse-long f2)]))

(defn fold
  [positions fold-cmd]
  (set (map #(fold-point fold-cmd %) positions)))

(comment
  (with-open [rdr (io/reader (io/resource "day_13_input.txt"))]
    (let [[positions folds] (->> (line-seq rdr)
                              (partition-by #(empty? %))
                              (filter #(not= (first %) "")))]
      (def result (reduce fold (map str->pos positions) (map str->fold folds)))))

  (map #(map first %) (map second (group-by second result)))
  (count result)

  ;;NOTE: Shrink the plot to see the pattern
  ;;NOTE: The letters are upside down
  (use '(incanter core stats charts io))
  (view (scatter-plot (map first result) (map second result)))

  )

