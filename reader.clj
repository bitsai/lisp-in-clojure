(ns reader
  (:require [clojure.string :as str]))

(defn tokenize [exp]
  (let [tokens (-> exp
		   (str/replace "(" " ( ")
		   (str/replace ")" " ) ")
		   (str/replace "'" " ' ")
		   (str/split #"\s"))]
    (filter (comp not empty?) tokens)))

(defn listify
  ([tokens] (first (second (listify tokens nil))))
  ([tokens lst]
     (let [head (first tokens)
	   tail (rest tokens)]
       (cond
	(empty? tokens) [nil lst]
	(= head "(") (let [[new-tokens sub-lst] (listify tail nil)
			   new-lst (concat lst (list sub-lst))]
		       (listify new-tokens new-lst))
	(= head ")") [tail lst]
	(= head "'") (let [[new-tokens sub-lst] (listify tail nil)
			   q (list "quote" (first sub-lst))
			   new-lst (concat lst (list q) (rest sub-lst))]
		       [new-tokens new-lst])
	:else (listify tail (concat lst (list head)))))))

(defn read* [exp] (listify (tokenize exp)))
