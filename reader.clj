(ns reader
  (:require [clojure.string :as str]))

(defn tokenize [exp]
  (let [tokens (-> exp
		   (str/replace "(" " ( ")
		   (str/replace ")" " ) ")
		   (str/replace "'" " ' ")
		   (str/split #"\s"))]
    (remove empty? tokens)))

(declare micro-read read-list)

(defn micro-read [[t & ts]]
  (cond
   (= t "(") (read-list [] ts)
   (= t "'") (let [[new-t new-ts] (micro-read ts)]
	       [["quote" new-t] new-ts])
   :else [t ts]))

(defn read-list [list-so-far tokens]
  (let [[t ts] (micro-read tokens)]
    (cond
     (= t ")") [list-so-far ts]
     (= t "(") (let [[new-list new-ts] (read-list [] ts)]
		 (read-list (conj list-so-far new-list) new-ts))
     :else (read-list (conj list-so-far t) ts))))

(defn read* [exp]
  (let [[nested-lists _] (micro-read (tokenize exp))]
    nested-lists))
