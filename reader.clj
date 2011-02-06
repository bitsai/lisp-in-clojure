(ns reader
  (:require [clojure.string :as str]))

(defn tokenize [exp]
  (let [tokens (-> exp
		   (str/replace "(" " ( ")
		   (str/replace ")" " ) ")
		   (str/replace "'" " ' ")
		   (str/split #"\s"))]
    (remove empty? tokens)))

(defn read-list [list-so-far [t & ts]]
  (cond
   (nil? t) [list-so-far nil]
   (= t ")") [list-so-far ts]
   (= t "(") (let [[sub-list next-ts] (read-list [] ts)]
	       (read-list (conj list-so-far sub-list) next-ts))
   (= t "'") (let [[[x & xs] next-ts] (read-list [] ts)]
	       [(apply conj list-so-far ["quote" x] xs) next-ts])
   :else (read-list (conj list-so-far t) ts)))

(defn micro-read [tokens]
  (ffirst (read-list [] tokens)))

(defn read* [exp] (micro-read (tokenize exp)))
