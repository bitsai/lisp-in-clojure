(ns tests
  (:require [eval-pg :as pg])
  (:require [eval-pm :as pm])
  (:use [reader :only (_read)]))

(def env [["true" true]
	  ["false" false]])

(defn _test
  ([exp-str] (_test exp-str ""))
  ([exp-str env-str] (let [test-exp (_read exp-str)
			   test-env (_read env-str)]
		       ;; (println (pm/_eval test-exp (concat test-env env)))
		       (println (pg/_eval test-exp (concat test-env env))))))

;; quote
(_test "(quote a)")
;;(_test "'a") ;; needs support for ' macro
(_test "(quote (a b c))")

;; atom
(_test "(atom (quote a))")
(_test "(atom (quote (a b c)))")
(_test "(atom (quote ()))")
(_test "(atom (atom (quote a)))")
(_test "(atom (quote (atom (quote a))))")

;; eq
(_test "(eq (quote a) (quote a))")
(_test "(eq (quote a) (quote b))")
(_test "(eq (quote ()) (quote ()))")

;; car
(_test "(car (quote (a b c)))")

;; cdr
(_test "(cdr (quote (a b c)))")

;; cons
(_test "(cons (quote a) (quote (b c)))")
(_test "(cons (quote a) (cons (quote b) (cons (quote c) (quote ()))))")
(_test "(car (cons (quote a) (quote (b c))))")
(_test "(cdr (cons (quote a) (quote (b c))))")

;; cond
(_test (str "(cond ((eq (quote a) (quote b)) (quote first))"
	    "      ((atom (quote a)) (quote second)))"))

;; lambda
(_test "((lambda (x) (cons x (quote (b)))) (quote a))")
(_test (str "((lambda (x y) (cons x (cdr y)))"
	    " (quote z)"
	    " (quote (a b c)))"))
(_test (str "((lambda (f) (f (quote (b c))))"
	    " (quote (lambda (x) (cons (quote a) x))))"))

;; label
(_test "(subst (quote m) (quote b) (quote (a b (a b c) d)))"
       (str "((subst (label subst (lambda (x y z)"
	    "                       (cond ((atom z)"
	    "                              (cond ((eq z y) x)"
	    "                                    (true z)))"
	    "                             (true (cons (subst x y (car z))"
	    "                                         (subst x y (cdr z)))))))))"))

;; eval
(_test "x"
       "((x a) (y b))")
(_test "(eq (quote a) (quote a))")
(_test "(cons x (quote (b c)))"
       "((x a) (y b))")
(_test (str "(cond ((atom x) (quote atom))"
	    "      (true (quote list)))")
       "((x (quote (a b))))")
(_test "(f (quote (b c)))"
       "((f (lambda (x) (cons (quote a) x))))")
(_test (str "((label firstatom (lambda (x)"
	    "                    (cond ((atom x) x)"
	    "                          (true (firstatom (car x))))))"
	    " y)")
       "((y ((a b) (c d))))")
(_test (str "((lambda (x y) (cons x (cdr y)))"
	    " (quote a)"
	    " (quote (b c d)))"))
