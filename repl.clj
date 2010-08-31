(ns repl
  (:use [interpreter :only (env evaluate)])
  (:use [reader :only (tokenize parse)]))

(println "REPL started!")

(let [line (read-line)
      tokens (tokenize line)]
  (when (not= (first tokens) "exit")
    (println (evaluate (parse line) env))
    (recur)))
