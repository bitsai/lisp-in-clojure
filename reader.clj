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
  ([tokens] (ffirst (listify [] tokens)))
  ([l [t & ts :as tokens]]
     (cond
      (empty? tokens) [l nil]
      (= t "(") (let [[sub-l new-tokens] (listify [] ts)]
		  (listify (conj l sub-l) new-tokens))
      (= t ")") [l ts]
      (= t "'") (let [[[x & xs] new-tokens] (listify [] ts)
		      new-l (apply conj l ["quote" x] xs)]
		  [new-l new-tokens])
      :else (listify (conj l t) ts))))

(defn read* [exp] (listify (tokenize exp)))
