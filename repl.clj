(ns repl
  (:use [env :only (make-env)])
  (:use [eval :only (eval*)])
  (:use [reader :only (read*)]))

(println "REPL started!")

(loop [env (make-env)]
  (let [exp (read-line)]
    (if-not (empty? exp)
      (println (eval* (read* exp) env))))
  (recur env))
