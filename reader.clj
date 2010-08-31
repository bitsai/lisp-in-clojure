(ns reader
  (:require [clojure.string :as str]))

(defn _tokenize [line]
  (let [tokens (-> line
		   (str/replace "(" " ( ")
		   (str/replace ")" " ) ")
		   (str/split #"\s"))]
    (filter (comp not empty?) tokens)))

(defn _parse
  ([tokens] (_parse '() tokens))
  ([acc tokens]
     (let [head (first tokens)
	   tail (rest tokens)]
       (cond
	(empty? tokens) (first acc)
	(= head ")") [acc tail]
	(= head "(") (let [[sub-acc new-tokens] (_parse tail)]
		       (_parse (concat acc (list sub-acc)) new-tokens))
	:else (_parse (concat acc (list head)) tail)))))

(defn _read [line] (_parse (_tokenize line)))
