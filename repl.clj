(ns repl
  (:use [eval :only (env eval*)])
  (:use [reader :only (read* tokenize)]))

(println "REPL started!")

(let [exp (read-line)
      tokens (tokenize exp)]
  (when (not= (first tokens) "exit")
    (println (eval* (read* exp) env))
    (recur)))
