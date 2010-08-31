(ns repl
  (:use [interpreter :only (_eval)])
  (:use [reader :only (tokenize _read)]))

(def env [["true" true]
	  ["false" false]])

(defn repl []
  (let [exp-str (read-line)
	tokens (tokenize exp-str)]
    (if (not= (first tokens) "exit")
      (let [exp (_read exp-str)
	    result (_eval exp env)]
	(println result)
	(recur)))))

(println "REPL started!")

(repl)
