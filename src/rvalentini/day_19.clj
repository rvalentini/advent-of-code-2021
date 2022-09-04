(ns rvalentini.day_19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :refer [keywordize-keys]]))

(def variants [[1 1 1] [-1 1 1] [1 -1 1] [1 1 -1] [-1 -1 1] [1 -1 -1] [-1 1 -1] [-1 -1 -1]])

(defn reading->coordinates [reading]
  (map #(Integer/parseInt %) (str/split reading #",")))

(defn combine [[a1 a2 a3] [b1 b2 b3]] [(- a1 b1) (- a2 b2) (- a3 b3)])

(defn locate-centers [scanner1 scanner2]
  (for [s1 scanner1
        s2 scanner2]
    (combine s1 s2)))

(defn all-rotations [readings]
  (let [shift (fn [[a b c]] [[a b c] [a c b] [b a c] [b c a] [c a b] [c b a]])
        switch (fn [r] (for [v variants] (map * r v)))
        rotations (fn [r] (for [s (shift r)
                                sw (switch s)]
                            sw))]
    (apply map vector (map rotations readings))))

(defn some-aligned? [frequencies]
  (some #(when (< 11 (val %)) (key %)) frequencies))

(defn relative->absolute [[p1 p2 p3] [c1 c2 c3]]
  [(+ c1 p1) (+ c2 p2) (+ c3 p3)])

(defn find-aligned-rotation [scanner rotation index]
  (let [centers (locate-centers (:readings scanner) rotation)
        frqs (frequencies centers)]
    (when-let [center (some-aligned? frqs)]
      {:center center
       :readings (map (fn [r] (relative->absolute r center)) rotation)
       :index index})))

(defn find-aligned-scanners [base-scanner all-scanners]
  (let [scanners (dissoc all-scanners (:index base-scanner))]
    (loop [[idx & rest] (keys scanners)
           aligned []]
      (if idx
        (recur
          rest
          (conj aligned (some
                          #(find-aligned-rotation base-scanner % idx)
                          (all-rotations (idx scanners)))))
        (filter some? aligned)))))

(defn explore-scanners [scanners]
  (loop [original-scanners scanners
         aligned-scanners [{:index :0
                            :readings (:0 original-scanners)
                            :center [0 0 0]}]
         explored-idx []]
    (if-let [scanner (some #(when-not (some #{(:index %)} explored-idx) %) aligned-scanners)]
      (let [new-aligned (find-aligned-scanners scanner original-scanners)]
        (recur
          (dissoc original-scanners (map :index new-aligned))
          (apply conj aligned-scanners new-aligned)
          (conj explored-idx (:index scanner))))
      aligned-scanners)))

(defn manhattan [[a1 a2 a3] [b1 b2 b3]]
  (Math/abs (+ (- a1 b1) (- a2 b2) (- a3 b3))))

(defn max-manhattan [centers]
  (apply max (for [p1 centers
                   p2 centers]
               (manhattan p1 p2))))

(comment
  (with-open [rdr (io/reader (io/resource "day_19_input.txt"))]
    (let [scanners (some->> (line-seq rdr)
                     (partition-by #(str/starts-with? "---" %))
                     (map rest)
                     (filter seq)
                     (map #(map reading->coordinates %))
                     (map-indexed (fn [k v] [(str k) v]))
                     (into (sorted-map))
                     keywordize-keys
                     doall)
          result (into (hash-set) (explore-scanners scanners))]

      (println "Total number of beacons: "
        (->> result
          (map :readings)
          (apply concat)
          (into (hash-set))
          count))

      (println "Maximum Manhattan distance:"
        (max-manhattan (map :center result)))))
  )
