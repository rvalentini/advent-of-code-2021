(ns rvalentini.day_12
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn is-small-cave? [cave] (not (= (s/upper-case cave) cave)))

(defn get-directly-connected-caves
  [edges cave]
  (->> edges
    (filter #(some #{cave} %))
    (map #(filter (fn [x] (not= x cave)) %))
    flatten))

(defn is-cave-allowed?
  [cave previous-path joker]
  (let [visit-count (->> previous-path (filter #{cave}) count)
        allowed-visits (if (= cave joker) 1 0)]
    (not (and (< allowed-visits visit-count) (is-small-cave? cave)))))

(defn find-next-caves
  [edges previous-path joker]
  (->> (first previous-path)
    (get-directly-connected-caves edges)
    (filter #(is-cave-allowed? % previous-path joker))))

(defn find-paths
  [edges source dest joker]
  (loop [paths '()
         partial-paths [(list source)]]
    (if (empty? partial-paths)
      paths
      (let [extended (apply concat (for [pp partial-paths]
                                     (map #(conj pp %) (find-next-caves edges pp joker))))
            {new-paths true new-partials false} (group-by #(= (first %) dest) extended)]
        (recur (concat paths new-paths) new-partials)))))

(defn get-joker-candidates
  [edges]
  (->> edges
    flatten
    set
    (filter #(not= % "end"))
    (filter #(not= % "start"))
    (filter is-small-cave?)))

(comment
  (with-open [rdr (io/reader (io/resource "day_12_input.txt"))]
    (let [edges (->> (line-seq rdr)
                  (map #(s/split % #"-")))
          jokers (get-joker-candidates edges)]
      (println "Number of paths: " (count (find-paths edges "start" "end" nil)))

      (println "Number of paths with single joker cave: "
        (->> jokers
          (map #(find-paths edges "start" "end" %))
          (apply concat)
          set
          count))))
  )
