(ns repl
  (:use [env :only (make-env)])
  (:use [eval :only (eval*)])
  (:use [reader :only (read*)]))

(println "REPL started!")

(loop [env (make-env)]
  (if-let [chars (seq (read-line))]
    (try
      (println (eval* (read* (apply str chars)) env))
      (catch Exception ex
	(println (.getMessage ex)))))
  (recur env))
