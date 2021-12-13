(ns rvalentini.day_1
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn count-inc
  "Count the number of times a value increases"
  [[count last] curr]
  [(if (< last curr)
     (inc count)
     count)
   curr])

(defn count-inc-windowed
  "Count the number of times the sum of the values inside a sliding window increase"
  [[count last] curr]
  [(if (< (apply + last) (apply + curr))
     (inc count)
     count)
   curr])


(comment
  (with-open [rdr (io/reader (io/resource "day_1_input.txt"))]
    (reduce count-inc [0 ##Inf] (map edn/read-string (line-seq rdr))))

  (with-open [rdr (io/reader (io/resource "day_1_input.txt"))]
    (reduce count-inc-windowed [0 '(##Inf)] (partition 3 1 (map edn/read-string (line-seq rdr)))))
  )



