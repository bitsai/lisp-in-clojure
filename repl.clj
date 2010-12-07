(ns repl
  (:use [eval :only (env eval*)])
  (:use [reader :only (read* tokenize)]))

(println "REPL started!")

(let [exp (read-line)
      first-word (first (tokenize exp))]
  (when-not (= first-word "exit")
    (println (eval* (read* exp) env))
    (recur)))
