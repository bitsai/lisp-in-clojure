(ns repl
  (:require [eval-pg :as pg])
  (:require [eval-pm :as pm])
  (:use [reader :only (tokenize _read)]))

(def env [["true" true]
	  ["false" false]])

(defn repl []
  (let [exp-str (read-line)
	tokens (tokenize exp-str)]
    (if (not= (first tokens) "exit")
      (let [exp (_read exp-str)]
	;; (println (pm/_eval (_read exp-str) env))
	(println (pg/_eval exp env))
	(recur)))))

(println "REPL started!")

(repl)
