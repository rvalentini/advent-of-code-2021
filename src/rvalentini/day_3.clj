(ns rvalentini.day_3
  (:require [clojure.java.io :as io]))

(defn bit-count
  [[zeros ones] curr]
  (case curr
    \1 [zeros (inc ones)]
    \0 [(inc zeros) ones]))

(defn bin->dec [bin] (Integer/parseInt bin 2))

(defn gamma-eval [[zeros ones]] (if (< zeros ones) 1 0))

(defn epsilon-eval [[zeros ones]] (if (>= zeros ones) 1 0))

(defn to-rate
  [bit-counts eval-fn]
  (bin->dec (apply str (map eval-fn bit-counts))))

(comment
  (with-open [rdr (io/reader (io/resource "day_3_input.txt"))]
    (let [bit-counts (map #(reduce bit-count [0 0] %) (apply mapv vector (map seq (line-seq rdr))))
          gamma-rate (to-rate bit-counts gamma-eval)
          epsilon-rate (to-rate bit-counts epsilon-eval)]
      (* gamma-rate epsilon-rate)))

  )



