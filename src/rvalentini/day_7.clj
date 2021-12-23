(ns rvalentini.day_7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn update-state
  [state position]
  (assoc state position (inc (get state position))))

(defn init-state
  [min max]
  (zipmap
    (range min (inc max))
    (repeat 0)))

(defn fuel-consumption
  [pos1 pos2 crab-count]
  (* (Math/abs (- pos1 pos2)) crab-count))

(defn fuel-consumption-2
  [pos1 pos2 crab-count]
  (* (apply + (range 1 (inc (Math/abs (- pos1 pos2))))) crab-count))

(defn overall-fuel-consumption
  [state fuel-fn position]
  [position (apply + (for [[current-pos crab-count] state]
              (fuel-fn position current-pos crab-count)))])

(defn find-optimal-pos
  [state fuel-fn]
  (->> (keys state)
    (map #(overall-fuel-consumption state fuel-fn %))
    (sort-by second)
    first))

(comment
  (with-open [rdr (io/reader (io/resource "day_7_input.txt"))]
    (let [positions (->> (line-seq rdr)
                      (map #(s/split % #","))
                      (map (fn [t] (map #(Integer/parseInt %) t)))
                      flatten)
          min-pos (apply min positions)
          max-pos (apply max positions)
          state (reduce update-state (init-state min-pos max-pos) positions)]
      (println "Part 1 - optimal position and fuel consumption:"
        (find-optimal-pos state fuel-consumption))

      (println "Part 2 - optimal position and fuel consumption:"
        (find-optimal-pos state fuel-consumption-2))))
  )



