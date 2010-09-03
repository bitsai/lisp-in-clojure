(ns repl
  (:use [eval :only (env eval*)])
  (:use [reader :only (read* tokenize)]))

(println "REPL started!")

(loop [line (read-line)
       tokens (tokenize line)]
  (when (not= (first tokens) "exit")
    (println (eval* (read* line) env))
    (recur)))
