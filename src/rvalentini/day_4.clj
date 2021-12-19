(ns rvalentini.day_4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defn mark-at-coordinates
  [board [row col]]
  (if (and row col)
    (let [[r1 r2] (split-at row board)
          [c1 c2] (split-at col (first r2))]
      (concat r1
        (list (concat c1 (list {:val (:val (first c2)) :marked true}) (drop 1 c2)))
        (drop 1 r2)))
    board))

(defn get-coordinates-of-n
  [board n]
  (first (->> (for [[x row] (map-indexed vector board)
                    [y val] (map-indexed vector row)]
                [[x y] val])
           (filter #(= (:val (second %)) n))
           (map first))))

(defn mark-board
  [board n]
  (mark-at-coordinates board (get-coordinates-of-n board n)))

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

(defn first-winner-reducer
  [boards n]
  (let [marked-boards (map #(mark-board % n) boards)
        winner (first (filter is-winner? marked-boards))]
    (if (some? winner)
      (reduced [winner n])
      marked-boards)))

(defn last-winner-reducer
  [[winners boards] n]
  (let [marked-boards (map #(mark-board % n) boards)
        grouped (group-by is-winner? marked-boards)]
    [(concat winners (map #(identity {:board % :n n}) (get grouped true))) (get grouped nil)]))

(comment
  (with-open [rdr (io/reader (io/resource "day_4_input.txt"))]
    (let [[first-line & rest] (line-seq rdr)
          numbers (str/split first-line #",")
          boards (partition 5 (->> rest
                                (filter #(not= % ""))
                                (map #(str/split (str/trim %) #"\s+"))
                                (map #(map (fn [n] {:val n :marked false}) %))))
          [first-winner n] (reduce first-winner-reducer boards numbers)
          last-winner (last (first (reduce last-winner-reducer ['() boards] numbers)))]

      (println "First to win:")
      (print-pretty first-winner)
      (println "Score:" (* (Integer/parseInt n 10) (calculate-score first-winner)))

      (println "Last to win:")
      (print-pretty (:board last-winner))
      (println "Score:" (* (Integer/parseInt (:n last-winner) 10) (calculate-score (:board last-winner))))))

  )



