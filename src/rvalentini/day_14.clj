(ns rvalentini.day_14
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn insert
  [pair element]
  (str (first pair) element (second pair)))

(defn match
  [pairs [pair target]]
  (map #(if (= % pair) (insert % target) %) pairs))

(defn merge-pairs
  [result [_ & rest]]
  (str result (apply str rest)))

(defn apply-rules
  [template rules]
  (let [pairs (map #(apply str %) (partition 2 1 template))
        [first & rest] (reduce match pairs rules)]
    (reduce merge-pairs first rest)))

(defn str->rule
  [str]
  (map #(s/trim %) (s/split str #"->")))

(defn element-frequency
  [counts element]
  (if (contains? counts element)
    (update counts element inc)
    (assoc counts element 1)))

(comment
  (with-open [rdr (io/reader (io/resource "day_14_input.txt"))]
    (let [[s1 s2] (->> (line-seq rdr)
                    (partition-by #(empty? %))
                    (filter #(not= (first %) "")))
          template (first s1)
          rules (map str->rule s2)
          step-fn (iterate #(apply-rules % rules) template)
          result (last (take 11 step-fn))
          frequencies (reduce element-frequency {} result)
          highest (second (first (sort-by val > frequencies)))
          lowest (second (last (sort-by val > frequencies)))]

      (println "Highest - lowest element frequency after 10 iterations: "
        (- highest lowest))))
  )

