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
  ([acc tokens]
     (let [head (first tokens)
	   tail (rest tokens)]
       (cond
	(empty? tokens) (first acc)
	(= head ")") [acc tail]
	(= head "(") (let [[new-acc new-tokens] (listify tail)]
		       (listify (concat acc (list new-acc)) new-tokens))
	:else (listify (concat acc (list head)) tail)))))

(defn parse [line] (listify (tokenize line)))
