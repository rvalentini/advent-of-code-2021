(ns rvalentini.day_14
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn to-pair-map
  [template]
  (->> template
    (map str)
    ((fn [l] [l (rest l)]))
    ((fn [[a b]] (map str a b)))
    (map #(identity {% 1}))
    (into {})))

(defn splice
  [pair target]
  [(str (first pair) target)
   (str target (last pair))])

(defn splice-update [state [pair cnt] target ]
  (let [[p1 p2] (splice pair target)]
    (-> state
      (update pair #(- (or % 0) cnt))
      (update p1 #(+ (or % 0) cnt))
      (update p2 #(+ (or % 0) cnt)))))

(defn splice-reducer
  [[state rules] element]
  [(if-let [target (rules (first element) )]
     (into {} (filter #(-> % val pos?) (splice-update state element target)))
     state) rules])

(defn first-char-reducer
  [agg [pair cnt]]
  (update agg (str (first pair)) #(+ (or % 0) cnt)))

(defn to-char-counts
  [state]
  (reduce first-char-reducer {} (seq state)))

(defn step
  [[state rules]]
  (reduce splice-reducer [state rules] (seq state)))

(defn str->rule
  [str]
  (map #(s/trim %) (s/split str #"->")))

(defn most-frequent [state] (reduce max (map val (seq state))))

(defn least-frequent [state] (reduce min (map val (seq state))))

(comment
  (with-open [rdr (io/reader (io/resource "day_14_input.txt"))]
    (let [[s1 s2] (->> (line-seq rdr)
                    (partition-by #(empty? %))
                    (filter #(not= (first %) "")))
          template (first s1)
          rules (into {} (map vec (map str->rule s2)))
          state (to-pair-map template)
          last-char (last template)
          [final-state _] (last (take 41 (iterate step [state rules])))
          counts (update (to-char-counts final-state) (str last-char) inc)]
      (println "Difference between most frequent and least frequent char counts: "
        (- (most-frequent counts) (least-frequent counts)))))
  )

