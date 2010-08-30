(ns tests
  (:require [eval-pg :as pg])
  (:require [eval-pm :as pm])
  (:use [reader :only (_read)]))

(def env [["true" true]
	  ["false" false]])

(defn test-eval
  ([exp-str] (test-eval exp-str ""))
  ([exp-str env-str] (let [test-exp (_read exp-str)
			   test-env (_read env-str)]
		       ;; (println (pm/_eval test-exp (concat test-env env)))
		       (println (pg/_eval test-exp (concat test-env env))))))

;; quote
(test-eval "(quote a)")
;;(test-eval "'a") ;; needs support for ' macro
(test-eval "(quote (a b c))")

;; atom
(test-eval "(atom (quote a))")
(test-eval "(atom (quote (a b c)))")
(test-eval "(atom (quote ()))")
(test-eval "(atom (atom (quote a)))")
(test-eval "(atom (quote (atom (quote a))))")

;; eq
(test-eval "(eq (quote a) (quote a))")
(test-eval "(eq (quote a) (quote b))")
(test-eval "(eq (quote ()) (quote ()))")

;; car
(test-eval "(car (quote (a b c)))")

;; cdr
(test-eval "(cdr (quote (a b c)))")

;; cons
(test-eval "(cons (quote a) (quote (b c)))")
(test-eval "(cons (quote a) (cons (quote b) (cons (quote c) (quote ()))))")
(test-eval "(car (cons (quote a) (quote (b c))))")
(test-eval "(cdr (cons (quote a) (quote (b c))))")

;; cond
(test-eval (str "(cond ((eq (quote a) (quote b)) (quote first))"
		"      ((atom (quote a)) (quote second)))"))

;; lambda
(test-eval "((lambda (x) (cons x (quote (b)))) (quote a))")
(test-eval (str "((lambda (x y) (cons x (cdr y)))"
		" (quote z)"
		" (quote (a b c)))"))
(test-eval (str "((lambda (f) (f (quote (b c))))"
		" (quote (lambda (x) (cons (quote a) x))))"))

;; label
(test-eval (str "((label subst (lambda (x y z)"
		"                (cond ((atom z)"
		"                       (cond ((eq z y) x)"
		"                             (true z)))"
		"                      (true (cons (subst x y (car z))"
		"                                  (subst x y (cdr z)))))))"
		" (quote m)"
		" (quote b)"
		" (quote (a b (a b c) d)))"))

;; eval
(test-eval "x"
	   "((x a) (y b))")
(test-eval "(eq (quote a) (quote a))")
(test-eval "(cons x (quote (b c)))"
	   "((x a) (y b))")
(test-eval (str "(cond ((atom x) (quote atom))"
		"      (true (quote list)))")
	   "((x (quote (a b))))")
(test-eval "(f (quote (b c)))"
	   "((f (lambda (x) (cons (quote a) x))))")
(test-eval (str "((label firstatom (lambda (x)"
		"                    (cond ((atom x) x)"
		"                          (true (firstatom (car x))))))"
		" y)")
	   "((y ((a b) (c d))))")
(test-eval (str "((lambda (x y) (cons x (cdr y)))"
		" (quote a)"
		" (quote (b c d)))"))
