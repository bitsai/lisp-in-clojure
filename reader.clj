(ns reader
  (:require [clojure.string :as str]))

(defn tokenize [line]
  (let [tokens (-> line
		   (str/replace "(" " ( ")
		   (str/replace ")" " ) ")
		   (str/split #"\s"))]
    (filter (comp not empty?) tokens)))

(defn listify
  ([tokens] (listify nil tokens))
  ([l tokens]
     (let [head (first tokens)
	   tail (rest tokens)]
       (cond
	(empty? tokens) (first l)
	(= head ")") [l tail]
	(= head "(") (let [[new-l new-tokens] (listify tail)]
		       (listify (concat l (list new-l)) new-tokens))
	:else (listify (concat l (list head)) tail)))))

(defn parse [line] (listify (tokenize line)))
