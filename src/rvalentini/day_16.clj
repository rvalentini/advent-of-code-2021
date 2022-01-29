(ns rvalentini.day_16
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(declare apply-operator)

(def config
  {:version            [0 3]
   :type               [3 6]
   :length-type-id     [6 7]
   :sub-count          [7 18]
   :sub-length         [7 22]
   :min-package-length 11})

(def ops
  {0 +
   1 *
   2 min
   3 max
   5 #(if (< %1 %2) 1 0)
   6 #(if (> %1 %2) 1 0)
   7 #(if (= %1 %2) 1 0)})

(defn calculate-padding
  [package-length]
  (let [offset (mod package-length 4)]
    (if (zero? offset) 0 (- 4 offset))))

(defn hex->bin
  [hex]
  (let [binary (Integer/toString (Integer/parseInt hex 16) 2)]
    (str (apply str (take (calculate-padding (count binary)) (repeat "0"))) binary)))

(defn bin->dec [bin] (BigInteger. bin 2))

(defn extract [input tag]
  (let [[s e] (config tag)]
    (subs input s e)))

(defmulti parse-package
  (fn [input]
    (case (extract input :type)
      "100" :literal
      :operator)))

(defn packages-only
  [input]
  (when (>= (count input) (config :min-package-length)) input))

(defn parse-sub-packages-by-length
  [input]
  (let [sub-length (bin->dec (extract input :sub-length))
        payload-start (second (config :sub-length))
        payload-end (+ payload-start sub-length)
        payload (subs input payload-start payload-end)
        rest (drop payload-end input)]
    (loop [payload payload
           packages '()]
      (if (empty? payload)
        [packages (packages-only (apply str rest))]
        (let [[package rest] (parse-package payload)]
          (recur rest (conj packages package)))))))

(defn parse-sub-packages-by-count
  [input]
  (let [sub-count (bin->dec (extract input :sub-count))
        payload-start (second (config :sub-count))]
    (loop [packages-left sub-count
           payload (drop payload-start input)
           packages '()]
      (if (pos? packages-left)
        (let [[package rest] (parse-package (apply str payload))]
          (recur (dec packages-left) rest (conj packages package)))
        [packages payload]))))

(defmethod parse-package :operator
  [input]
  (let [length-type (extract input :length-type-id)
        [result rest] (if (= length-type "1")
                        (parse-sub-packages-by-count input)
                        (parse-sub-packages-by-length input))]
    [{:version (bin->dec (extract input :version))
      :type    (ops (bin->dec (extract input :type)))
      :subs    result} rest]))

(defmethod parse-package :literal
  [input]
  (loop [[prefix & payload] (drop (second (config :type)) input)
         digits '()
         position 11]
    (let [digits-new (concat digits (take 4 payload))
          remaining (drop 4 payload)]
      (if (= prefix \0)
        [{:version (bin->dec (extract input :version))
          :type    (ops (bin->dec (extract input :type)))
          :literal (bin->dec (apply str digits-new))}
         (packages-only (apply str remaining))]
        (recur remaining digits-new (+ position 5))))))

(defn parse-message
  [msg]
  (loop [packages '()
         input msg]
    (let [[package rest] (parse-package input)]
      (if (empty? rest)
        (conj packages package)
        (recur (conj packages package) rest)))))

(defn sum-up-versions
  [package]
  (loop [versions '()
         [x & xs] (list package)]
    (if (nil? x)
      (reduce + versions)
      (recur (conj versions (x :version)) (if (some? (x :subs)) (concat xs (x :subs)) xs)))))

(defn calculate-value
  [package]
  (if-let [subs (package :subs)]
    (apply (package :type) (map calculate-value subs))
    (package :literal)))

(comment
  (with-open [rdr (io/reader (io/resource "day_16_input.txt"))]
    (let [input (->> (line-seq rdr)
                  (map #(s/split % #""))
                  first
                  (map hex->bin)
                  (apply str))
          parsed (first (parse-message input))]
      (println "Sum of all versions: " (sum-up-versions parsed))
      (println "Final value: " (calculate-value parsed))))
  )
