(ns rvalentini.day_17
  (:require [clojure.java.io :as io]))

(defn step
  [axis [i pos vel]]
  (case axis
    :x [(inc i) (+ pos vel) (if (pos? vel) (max (dec vel) 0) (min (inc vel) 0))]
    :y [(inc i) (+ pos vel) (dec vel)]))

(defn is-hit?
  [axis [_ pos _] target]
  (<= (get-in target [axis :start]) pos (get-in target [axis :end])))

(defn is-on-target? [[_ pos _] target] (>= pos (get-in target [:y :start])))

(defn steps->hits
  [axis steps target]
  (filter #(is-hit? axis % target) steps))

(defn vel->max-y-pos
  [vel target]
  (let [steps (->> [0 0 vel]
                (iterate #(step :y %))
                (take-while (fn [x] (is-on-target? x target))))]
    (when (seq (steps->hits :y steps target))
      (reduce #(max %1 (second %2)) 0 steps))))

(defn vel->hits
  ([axis vel target]
   (vel->hits axis vel target nil))
  ([axis vel target max-i]
   (let [steps (->> [0 0 vel]
                 (iterate #(step axis %))
                 (#(if (= axis :x)
                     (take max-i %)
                     (take-while (fn [x] (is-on-target? x target)) %))))]
     (reduce #(conj %1 [(first %2) vel]) '() (steps->hits axis steps target)))))

(defn compute-step-vel-map
  ([axis range target]
   (compute-step-vel-map axis range target nil))
  ([axis range target max-i]
   (->> range
     (map #(vel->hits axis % target max-i))
     (filter seq)
     (apply concat)
     (reduce (fn [acc [k v]]
               (update acc k #(if (nil? %)
                                (list v)
                                (conj % v)))) (sorted-map)))))

(defn join-velocities-by-steps
  [x-steps y-steps]
  (set (apply concat (map
                       (fn [[x-step x-vels]]
                         (for [x-vel x-vels
                               y-vel (y-steps x-step)]
                           [x-vel y-vel]))
                       (seq x-steps)))))

(comment
  (with-open [rdr (io/reader (io/resource "day_17_input.txt"))]
    (let [[x-start x-end y-start y-end] (->> (line-seq rdr)
                                          first
                                          (re-seq #"-?\d+")
                                          (map parse-long))
          target {:x {:start x-start :end x-end}
                  :y {:start y-start :end y-end}}
          max-x (get-in target [:x :end])
          max-y (abs (get-in target [:y :start]))
          y-steps (compute-step-vel-map :y (range (- max-y) max-y) target)
          max-i (inc (reduce (fn [acc [k _]] (max acc k)) 0 (seq y-steps)))
          x-steps (compute-step-vel-map :x (range 0 (inc max-x)) target  max-i)]

      (println "Highest y-position: "
        (last (map #(vel->max-y-pos % target) (range 0 max-y))))

      (println "Number of initial distinct velocities that will hit the target eventually: "
        (count (join-velocities-by-steps x-steps y-steps)))))
  )



