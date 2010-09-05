(ns tests
  (:use [eval :only (env eval*)])
  (:use [reader :only (read*)]))

(defn run-test [exp answer]
  (let [result (eval* (read* exp) env)]
    (if (= result answer) (println "OK")
	(println "FAIL"))))

(println "\n(QUOTE)")
(run-test "(quote a)"
	  "a")
(run-test "'a",
	  "a")
(run-test "(quote (a b c))"
	  ["a" "b" "c"])

(println "\n(ATOM)")
(run-test "(atom 'a)"
	  "t")
(run-test "(atom '(a b c))"
	  "f")
(run-test "(atom '())"
	  "t")
(run-test "(atom (atom 'a))"
	  "t")
(run-test "(atom '(atom 'a))"
	  "f")

(println "\n(EQ)")
(run-test "(eq 'a 'a)"
	  "t")
(run-test "(eq 'a 'b)"
	  "f")
(run-test "(eq '() '())"
	  "t")

(println "\n(CAR)")
(run-test "(car '(a b c))"
	  "a")

(println "\n(CDR)")
(run-test "(cdr '(a b c))"
	  ["b" "c"])

(println "\n(CONS)")
(run-test "(cons 'a '(b c))"
	  ["a" "b" "c"])
(run-test "(cons 'a (cons 'b (cons 'c '())))"
	  ["a" "b" "c"])
(run-test "(car (cons 'a '(b c)))"
	  "a")
(run-test "(cdr (cons 'a '(b c)))"
	  ["b" "c"])

(println "\n(COND)")
(run-test (str "(cond ((eq 'a 'b) 'first)"
	       "      ((atom 'a)  'second))")
	  "second")

(println "\n(LAMBDA)")
(run-test "((lambda (x) (cons x '(b))) 'a)"
	  ["a" "b"])
(run-test (str "((lambda (x y) (cons x (cdr y)))"
	       " 'z"
	       " '(a b c))")
	  ["z" "b" "c"])
(run-test (str "((lambda (f) (f '(b c)))"
	       " '(lambda (x) (cons 'a x)))")
	  ["a" "b" "c"])

(println "\n(DEFUN SUBST)")
(run-test "(subst 'm 'b '(a b (a b c) d))"
	  ["a" "m" ["a" "m" "c"] "d"])

(println "\n(DEFUN CAAR)")
(run-test "(caar '((a b x) (c d) e))"
	  "a")

(println "\n(DEFUN CADR)")
(run-test "(cadr '((a b x) (c d) e))"
	  ["c" "d"])

(println "\n(DEFUN CADAR)")
(run-test "(cadar '((a b x) (c d) e))"
	  "b")

(println "\n(DEFUN CADDR)")
(run-test "(caddr '((a b x) (c d) e))"
	  "e")

(println "\n(DEFUN CADDAR)")
(run-test "(caddar '((a b x) (c d) e))"
	  "x")

(println "\n(DEFUN TUPLE)")
(run-test "(tuple 'a 'b)"
	  ["a" "b"])

(println "\n(DEFUN NULL)")
(run-test "(null 'a)"
	  "f")
(run-test "(null '())"
	  "t")

(println "\n(DEFUN AND)")
(run-test "(and (atom 'a) (eq 'a 'a))"
	  "t")
(run-test "(and (atom 'a) (eq 'a 'b))"
	  "f")

(println "\n(DEFUN NOT)")
(run-test "(not (eq 'a 'a))"
	  "f")
(run-test "(not (eq 'a 'b))"
	  "t")

(println "\n(DEFUN APPEND)")
(run-test "(append '(a b) '(c d))"
	  ["a" "b" "c" "d"])
(run-test "(append '() '(c d))"
	  ["c" "d"])

(println "\n(DEFUN PAIR)")
(run-test "(pair '(x y z) '(a b c))"
	  [["x" "a"] ["y" "b"] ["z" "c"]])

(println "\n(DEFUN ASSOC)")
(run-test "(assoc 'x '((x a) (y b)))"
	  "a")
(run-test "(assoc 'x '((x new) (x a) (y b)))"
	  "new")

(println "\n(DEFUN EVAL)")
(run-test "(eval 'x '((x a) (y b)))"
	  "a")
(run-test "(eval '(eq 'a 'a) '())"
	  "t")
(run-test (str "(eval '(cons x '(b c))"
	       "      '((x a) (y b)))")
	  ["a" "b" "c"])
(run-test (str "(eval '(cond ((atom x) 'atom)"
	       "             ('t 'list))"
	       "      '((x '(a b))))")
	  "list")
(run-test (str "(eval '(f '(b c))"
	       "      '((f (lambda (x) (cons 'a x)))))")
	  ["a" "b" "c"])
(run-test (str "(eval '((label first (lambda (x)"
	       "                       (cond ((atom x) x)"
	       "                             ('t (first (car x))))))"
	       "        y)"
	       "      '((y ((a b) (c d)))))")
	  "a")
(run-test (str "(eval '((lambda (x y) (cons x (cdr y)))"
	       "        'a"
	       "        '(b c d))"
	       "      '())")
	  ["a" "c" "d"])
