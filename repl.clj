(ns repl
  (:use [interpreter :only (_eval env)])
  (:use [reader :only (_tokenize _read)]))

(defn _repl []
  (let [exp-str (read-line)
	tokens (_tokenize exp-str)]
    (if (not= (first tokens) "exit")
      (let [exp (_read exp-str)
	    result (_eval exp env)]
	(println result)
	(recur)))))

(println "REPL started!")

(_repl)
