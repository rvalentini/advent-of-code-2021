(ns rvalentini.day_18
  (:require [clojure.java.io :as io]
            [clojure.zip :as z]))

(defn needs-exploding?
  [loc]
  (when (vector? (z/node loc))
    (let [[l r] (z/node loc)]
      (and
        (and (number? l) (number? r))
        (>= (count (z/path loc)) 4)))))

(defn needs-splitting? [loc]
  (let [val (z/node loc)]
    (and (number? val) (>= val 10))))

(defn to-top
  [loc]
  (loop [loc loc]
    (if-let [up (z/up loc)]
      (recur up)
      loc)))

(def move
  {:prev z/prev
   :next z/next})

(defn other-direction
  [direction]
  (if (= direction :prev)
    (move :next)
    (move :prev)))

(defn walk-back
  [loc steps direction]
  (let [move (other-direction direction)]
    (reduce #(%2 %1) loc (repeat steps move))))

(defn add-to-neighbor
  [init-loc value direction]
  (loop [loc init-loc
         steps 0]
    (if (and (some? loc) (not (z/end? loc)))
      (if (number? (z/node loc))
        (walk-back (z/edit loc #(+ % value)) steps direction)
        (recur ((move direction) loc) (inc steps)))
      init-loc)))

(defn explode
  [loc]
  (let [node (z/node loc)]
    (if (every? number? node)
      (-> loc
        (z/replace :editing)                                ;prevents detection as next/prev leaf
        (add-to-neighbor (first node) :prev)
        (add-to-neighbor (second node) :next)
        (z/replace 0))
      (throw (Exception. "Can only explode leaf nodes!")))))

(defn split
  [loc]
  (let [val (z/node loc)]
    (if (number? val)
      (z/replace loc [(Math/floor (/ val 2)) (Math/ceil (/ val 2))])
      (throw (Exception. "Can only split numbers!")))))

(defn reduce-sfn
  [loc]
  (loop [loc loc
         mode :exploding]
    (case mode
      :exploding (if (z/end? loc)
                   (recur (z/vector-zip (z/root loc)) :splitting)
                   (recur (z/next (if (needs-exploding? loc) (explode loc) loc)) :exploding))
      :splitting (if (z/end? loc)
                   (z/root loc)
                   (if (needs-splitting? loc)
                     (recur (to-top (split loc)) :exploding)
                     (recur (z/next loc) :splitting))))))

(defn magnitude
  [loc]
  (loop [loc loc]
    (if (z/end? loc)
      (z/root loc)
      (let [node (z/node loc)]
        (if (and (vector? node) (every? number? node))
          (recur (to-top (z/replace loc (+ (* 3 (first node)) (* 2 (second node))))))
          (recur (z/next loc)))))))

(defn all-pairs
  [seq]
  (remove nil?
    (for [x seq y seq]
      (when (not= x y) [x y]))))

(comment
  (with-open [rdr (io/reader (io/resource "day_18_input.txt"))]
    (let [input (map (comp eval read-string) (line-seq rdr))
          reduced-sfn (reduce #(reduce-sfn (z/vector-zip [%1 %2])) input)]
      (println "Magnitude: " (magnitude (z/vector-zip reduced-sfn))))) ;3793

  (with-open [rdr (io/reader (io/resource "day_18_input.txt"))]
    (let [input (map (comp eval read-string) (line-seq rdr))
          pairs (all-pairs input)
          all-magnitudes (map (comp magnitude z/vector-zip reduce-sfn z/vector-zip) pairs)]
      (println "Max overall magnitude: " (reduce max all-magnitudes)))) ;4695
  )





