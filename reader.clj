(ns reader
  (:require [clojure.string :as str]))

(defn tokenize [exp]
  (let [tokens (-> exp
		   (str/replace "(" " ( ")
		   (str/replace ")" " ) ")
		   (str/replace "'" " ' ")
		   (str/split #"\s"))]
    (remove empty? tokens)))

(defn listify
  ([tokens] (ffirst (listify '() tokens)))
  ([acc [x & xs :as tokens]]
     (cond
      (empty? tokens) [acc nil]
      (= x "(") (let [[sub-list new-tokens] (listify nil xs)
		      new-acc (concat acc (list sub-list))]
		  (listify new-acc new-tokens))
      (= x ")") [acc xs]
      (= x "'") (let [[sub-list new-tokens] (listify nil xs)
		      quoted (list "quote" (first sub-list))
		      new-acc (concat acc (list quoted) (rest sub-list))]
		  [new-acc new-tokens])
      :else (listify (concat acc (list x)) xs))))

(defn read* [exp] (listify (tokenize exp)))
