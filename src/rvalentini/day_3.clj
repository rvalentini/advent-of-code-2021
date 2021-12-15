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

(defn filter-bit-at-pos
  [report bit pos]
  (filter #(= (nth % pos) bit) report))

(defn calculate-bit-counts
  [report]
  (map #(reduce bit-count [0 0] %) (apply mapv vector report)))

(defn oxygen-eval [[zeros ones]] (if (> zeros ones) \0 \1))

(defn c02-eval [[zeros ones]] (if (> zeros ones) \1 \0))

(defn calculate-rating
  [report eval-fn]
  (loop [report report
         bit-counts (calculate-bit-counts report)
         i 0]
    (if (= (count report) 1)
      (flatten report)
      (let [filtered (filter-bit-at-pos report (eval-fn (nth bit-counts i)) i)
            bit-counts (calculate-bit-counts filtered)]
        (recur filtered bit-counts (inc i))))))

(comment
  (with-open [rdr (io/reader (io/resource "day_3_input.txt"))]
    (let [report (map seq (line-seq rdr))
          bit-counts (calculate-bit-counts report)
          gamma-rate (to-rate bit-counts gamma-eval)
          epsilon-rate (to-rate bit-counts epsilon-eval)]
      (* gamma-rate epsilon-rate)))

  (with-open [rdr (io/reader (io/resource "day_3_input.txt"))]
    (let [report (map seq (line-seq rdr))
          oxygen-rating (bin->dec (apply str (calculate-rating report oxygen-eval)))
          c02-rating (bin->dec (apply str (calculate-rating report c02-eval)))]
      (* oxygen-rating c02-rating )))
  )



