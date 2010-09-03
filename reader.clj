(ns reader
  (:require [clojure.string :as str]))

(defn tokenize [line]
  (let [tokens (-> line
		   (str/replace "(" " ( ")
		   (str/replace ")" " ) ")
		   (str/replace "'" " ' ")
		   (str/split #"\s"))]
    (filter (comp not empty?) tokens)))

(defn listify
  ([tokens] (first (second (listify tokens nil))))
  ([tokens acc]
     (let [head (first tokens)
	   tail (rest tokens)]
       (cond
	(empty? tokens) [nil acc]
	(= head "(") (let [[new-tokens sub-acc] (listify tail nil)
			   new-acc (concat acc (list sub-acc))]
		       (listify new-tokens new-acc))
	(= head ")") [tail acc]
	(= head "'") (let [[new-tokens sub-acc] (listify tail nil)
			   quoted (list "quote" (first sub-acc))
			   new-acc (concat acc (list quoted) (rest sub-acc))]
		       [new-tokens new-acc])
	:else (listify tail (concat acc (list head)))))))

(defn parse [line] (listify (tokenize line)))
