(ns rvalentini.day_4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

;; DONT USE SPLIT AT but transform to vec and then use assoc
(defn mark-at-pos
  [board row col]
  (println "BOARD IN MARK POS: " board)
  (if (and row col)
    (let [[r1 r2] (split-at row board)
          [c1 c2] (split-at col (first r2))]
      (concat r1
        (list (concat c1 (list {:val (:val (first c2)) :marked true}) (drop 1 c2)))
        (drop 1 r2)))
    board))

(defn mark-number
  [board n]
  (println "BOARD IN MARK NUMBER: " board)
  (let [[row col _] (first (->> (for [[x row] (map-indexed vector board)
                                      [y val] (map-indexed vector row)]
                                  [x y val])
                             (filter #(= (:val (nth % 2)) n))))]
    (mark-at-pos board row col)))


(defn is-winner?
  [board]
  (or
    (some true? (->> board (map (fn [row] (every? #(true? (:marked %)) row)))))
    (some true? (->> (apply mapv vector board) (map (fn [col] (every? #(true? (:marked %)) col)))))))

(defn print-pretty [board]
  (pprint (for [row board]
            (for [n row]
              (if (:marked n)
                (str "*" (:val n) "*")
                (:val n))))))


(defn number-reducer
  [boards n]
  (println "BOARDS" boards)
  (let [marked-boards (map #(mark-number % n) boards)
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
                                (map #(map (fn [n] {:val n :marked false}) %))))]
      ;(println numbers)
      ;(println (second numbers))


      (print-pretty  (mark-number (first boards) "61"))
      (let [[n winner] (reduce number-reducer boards numbers)]
        (println "NUMBER:" n)
        (print-pretty winner)
        )



      ;(println boards)
      ;(println (is-winner? (first boards)))


      #_(let [board (first boards)
            winner (-> (first boards)
                     (mark-at-pos 0 2)
                     (mark-at-pos 1 2)
                     (mark-at-pos 2 2)
                     (mark-at-pos 3 2)
                     (mark-at-pos 4 2))]

        (print-pretty board)
        (println "IS WINNER? " (is-winner? board))
        (print-pretty winner)
        (println "IS WINNER? " (is-winner? winner)))
      ))

  )



