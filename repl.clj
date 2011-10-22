(ns repl
  (:require [env :as env]
            [eval :as eval]
            [reader :as reader]))

(println "REPL started!")

(loop [env (env/make-env)]
  (if-let [chars (seq (read-line))]
    (try
      (println (eval/eval* (reader/read* (apply str chars)) env))
      (catch Exception ex
	(println (.getMessage ex)))))
  (recur env))
