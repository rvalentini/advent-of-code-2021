(ns rvalentini.day_10
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def characters
  {"[" "]"
   "{" "}"
   "<" ">"
   "(" ")"})

(def score
  {")" 3
   "]" 57
   "}" 1197
   ">" 25137})

(def completion-score
  {")" 1
   "]" 2
   "}" 3
   ">" 4})

(defn calculate-completion-score
  [completion]
  (reduce #(+ (completion-score %2) (* %1 5)) 0 completion))

(defn complete
  [stack]
  (loop [completion '()
         [x & rest] (reverse stack)]
    (if (nil? x)
      completion
      (recur (conj completion (characters x)) rest))))

(defn parse-line
  [line]
  (loop [[x & rest] line
         stack '()]
    (cond
      (and (nil? x) (empty? stack)) :correct
      (and (nil? x) (seq stack)) stack
      :else (if (some? (characters x))
              (recur rest (conj stack x))
              (if (= (characters (peek stack)) x)
                (recur rest (pop stack))
                x)))))

(defn middle-element [xs] (nth xs (/ (count xs) 2)))

(comment
  (with-open [rdr (io/reader (io/resource "day_10_input.txt"))]
    (let [lines (->> (line-seq rdr)
                  (map #(s/split % #""))
                  (map parse-line))]
      (println "Syntax-error score: " (apply + (->> lines
                                                 (filter #(not (seq? %)))
                                                 (map #(score %)))))
      (println "Middle score: " (->> lines
                                  (filter #(seq? %))
                                  (map complete)
                                  (map calculate-completion-score)
                                  sort
                                  middle-element))))

  )



