(ns rvalentini.day_4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defn mark-at-pos
  [[row col] board]
  (if (and row col)
    (let [[r1 r2] (split-at row board)
          [c1 c2] (split-at col (first r2))]
      (concat r1
        (list (concat c1 (list {:val (:val (first c2)) :marked true}) (drop 1 c2)))
        (drop 1 r2)))
    board))

(defn get-pos-for-number
  [board n]
  (first (->> (for [[x row] (map-indexed vector board)
                    [y val] (map-indexed vector row)]
                [[x y] val])
           (filter #(= (:val (second %)) n))
           (map first))))

(defn all-marked?
  [seq]
  (every? #(true? (:marked %)) seq))

(defn is-winner?
  [board]
  (or
    (some true? (->> board (map all-marked?)))
    (some true? (->> (apply mapv vector board) (map all-marked?)))))

(defn print-pretty [board]
  (pprint (for [row board]
            (for [n row]
              (if (:marked n)
                (str "*" (:val n) "*")
                (:val n))))))

(defn calculate-score
  [board]
  (reduce + 0 (->> (flatten board)
                (filter #(false? (:marked %)))
                (map #(Integer/parseInt (:val %) 10)))))


(defn number-reducer
  [boards n]
  (let [marked-boards (map #(-> %
                              (get-pos-for-number n)
                              (mark-at-pos %)) boards)
        winner (first (filter is-winner? marked-boards))]
    (if (some? winner)
      (reduced [n winner])
      marked-boards)))

(comment
  (with-open [rdr (io/reader (io/resource "day_4_input.txt"))]
    (let [[first-line & rest] (line-seq rdr)
          numbers (str/split first-line #",")
          boards (partition 5 (->> rest
                                (filter #(not= % ""))
                                (map #(str/split % #"\s+"))
                                (map #(map (fn [n] {:val n :marked false}) %))))
          [n winner] (reduce number-reducer boards numbers)]

      (print-pretty winner)
      (println "Winner score:" (* (Integer/parseInt n 10) (calculate-score winner)))))
  )



