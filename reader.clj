(ns reader
  (:require [clojure.string :as str]))

(defn tokenize [exp]
  (remove empty? (-> exp
                     (str/replace "(" " ( ")
                     (str/replace ")" " ) ")
                     (str/replace "'" " ' ")
                     (str/split #"\s+"))))

(declare micro-read read-list)

(defn micro-read [[t & ts]]
  (case t
        "(" (read-list '() ts)
        "'" (let [[new-t new-ts] (micro-read ts)]
              [(list "quote" new-t) new-ts])
        [t ts]))

(defn read-list [list-so-far tokens]
  (let [[t ts] (micro-read tokens)]
    (case t
          ")" [(reverse list-so-far) ts]
          "(" (let [[new-list new-tokens] (read-list '() ts)]
                (read-list (cons new-list list-so-far) new-tokens))
          (read-list (cons t list-so-far) ts))))

(defn read* [exp]
  (first (micro-read (tokenize exp))))
