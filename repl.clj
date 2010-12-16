(ns repl
  (:use [eval :only (eval*)])
  (:use [env :only (make-env)])
  (:use [reader :only (read* tokenize)]))

(println "REPL started!")

(let [exp (read-line)
      first-word (first (tokenize exp))
      env (make-env)]
  (when-not (= first-word "exit")
    (println (eval* (read* exp) env))
    (recur)))
