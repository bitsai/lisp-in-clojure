(ns tests
  (:use [clojure.test :only (deftest are run-tests)])
  (:use [eval :only (eval*)])
  (:use [env :only (make-env)])
  (:use [reader :only (read*)]))

(let [env (make-env)]
  (deftest tests
    (are [exp answer] (= answer (eval* (read* exp) env))

	 "(quote a)"
	 "a"

	 "'a"
	 "a"

	 "(quote (a b c))"
	 ["a" "b" "c"]

	 "(atom 'a)"
	 "t"

	 "(atom '(a b c))"
	 "f"

	 "(atom '())"
	 "t"

	 "(atom (atom 'a))"
	 "t"

	 "(atom '(atom 'a))"
	 "f"

	 "(eq 'a 'a)"
	 "t"

	 "(eq 'a 'b)"
	 "f"

	 "(eq '() '())"
	 "t"

	 "(car '(a b c))"
	 "a"

	 "(cdr '(a b c))"
	 ["b" "c"]

	 "(cons 'a '(b c))"
	 ["a" "b" "c"]

	 "(cons 'a (cons 'b (cons 'c '())))"
	 ["a" "b" "c"]

	 "(car (cons 'a '(b c)))"
	 "a"

	 "(cdr (cons 'a '(b c)))"
	 ["b" "c"]

	 "(cond ((eq 'a 'b) 'first) ((atom 'a) 'second))"
	 "second"
	 
	 "((lambda (x) (cons x '(b))) 'a)"
	 ["a" "b"]

	 "((lambda (x y) (cons x (cdr y))) 'z '(a b c))"
	 ["z" "b" "c"]

	 "((lambda (f) (f '(b c))) '(lambda (x) (cons 'a x)))"
	 ["a" "b" "c"]

	 "(subst 'm 'b '(a b (a b c) d))"
	 ["a" "m" ["a" "m" "c"] "d"]

	 "(caar '((a b x) (c d) e))"
	 "a"

	 "(cadr '((a b x) (c d) e))"
	 ["c" "d"]

	 "(cadar '((a b x) (c d) e))"
	 "b"

	 "(caddr '((a b x) (c d) e))"
	 "e"

	 "(caddar '((a b x) (c d) e))"
	 "x"

	 "(tuple 'a 'b)"
	 ["a" "b"]

	 "(null 'a)"
	 "f"

	 "(null '())"
	 "t"

	 "(and (atom 'a) (eq 'a 'a))"
	 "t"

	 "(and (atom 'a) (eq 'a 'b))"
	 "f"

	 "(not (eq 'a 'a))"
	 "f"

	 "(not (eq 'a 'b))"
	 "t"

	 "(append '(a b) '(c d))"
	 ["a" "b" "c" "d"]

	 "(append '() '(c d))"
	 ["c" "d"]

	 "(pair '(x y z) '(a b c))"
	 [["x" "a"] ["y" "b"] ["z" "c"]]

	 "(assoc 'x '((x a) (y b)))"
	 "a"
	 
	 "(assoc 'x '((x new) (x a) (y b)))"
	 "new"

	 (str "(eval 'x"
	      "      '((x a) (y b)))")
	 "a"

	 (str "(eval '(eq 'a 'a)"
	      "      '())")
	 "t"

	 (str "(eval '(cons x '(b c))"
	      "      '((x a) (y b)))")
	 ["a" "b" "c"]
	 
	 (str "(eval '(cond ((atom x) 'atom) ('t 'list))"
	      "      '((x '(a b))))")
	 "list"
	 
	 (str "(eval '(f '(b c))"
	      "      '((f (lambda (x) (cons 'a x)))))")
	 ["a" "b" "c"]
	 
	 (str "(eval '((label first (lambda (x) (cond ((atom x) x)"
	      "                                       ('t (first (car x))))))"
	      "        y)"
	      "      '((y ((a b) (c d)))))")
	 "a"
	 
	 (str "(eval '((lambda (x y) (cons x (cdr y)))"
	      "        'a"
	      "        '(b c d))"
	      "      '())")
	 ["a" "c" "d"])))

(run-tests)
