(ns tests
  (:use [eval :only (env eval*)])
  (:use [reader :only (read*)]))

(defn test* [exp answer]
  (let [result (eval* (read* exp) env)]
    (if (= result answer) (println "OK")
      (println "FAIL"))))

(println "\n(QUOTE)")
(test* "(quote a)"
       "a")
(test* "'a",
       "a")
(test* "(quote (a b c))"
       ["a" "b" "c"])

(println "\n(ATOM)")
(test* "(atom 'a)"
       true)
(test* "(atom '(a b c))"
       false)
(test* "(atom '())"
       true)
(test* "(atom (atom 'a))"
       true)
(test* "(atom '(atom 'a))"
       false)

(println "\n(EQ)")
(test* "(eq 'a 'a)"
       true)
(test* "(eq 'a 'b)"
       false)
(test* "(eq '() '())"
       true)

(println "\n(CAR)")
(test* "(car '(a b c))"
       "a")

(println "\n(CDR)")
(test* "(cdr '(a b c))"
       ["b" "c"])

(println "\n(CONS)")
(test* "(cons 'a '(b c))"
       ["a" "b" "c"])
(test* "(cons 'a (cons 'b (cons 'c '())))"
       ["a" "b" "c"])
(test* "(car (cons 'a '(b c)))"
       "a")
(test* "(cdr (cons 'a '(b c)))"
       ["b" "c"])

(println "\n(COND)")
(test* (str "(cond ((eq 'a 'b) 'first)"
	    "      ((atom 'a)  'second))")
       "second")

(println "\n(LAMBDA)")
(test* "((lambda (x) (cons x '(b))) 'a)"
       ["a" "b"])
(test* (str "((lambda (x y) (cons x (cdr y)))"
	    " 'z"
	    " '(a b c))")
       ["z" "b" "c"])
(test* (str "((lambda (f) (f '(b c)))"
	    " '(lambda (x) (cons 'a x)))")
       ["a" "b" "c"])

(println "\n(DEFUN SUBST)")
(test* (str "(defun subst (x y z)"
	    "  (cond ((atom z)"
	    "         (cond ((eq z y) x)"
	    "               (true z)))"
	    "        (true (cons (subst x y (car z))"
	    "                    (subst x y (cdr z))))))")
       nil)
(test* "(subst 'm 'b '(a b (a b c) d))"
       ["a" "m" ["a" "m" "c"] "d"])

(println "\n(DEFUN CAAR)")
(test* "(defun caar (x) (car (car x)))"
       nil)
(test* "(caar '((a b) (c d) e))"
       "a")

(println "\n(DEFUN CADAR)")
(test* "(defun cadar (x) (car (cdr (car x))))"
       nil)
(test* "(cadar '((a b) (c d) e))"
       "b")

(println "\n(DEFUN NULL)")
(test* "(defun null (x) (eq x '()))"
       nil)
(test* "(null 'a)"
       false)
(test* "(null '())"
       true)

(println "\n(DEFUN AND)")
(test* (str "(defun and (x y)"
	    "  (cond (x (cond (y true) (true false)))"
	    "        (true false)))")
       nil)
(test* "(and (atom 'a) (eq 'a 'a))"
       true)
(test* "(and (atom 'a) (eq 'a 'b))"
       false)

(println "\n(DEFUN NOT)")
(test* (str "(defun not (x)"
	    "  (cond (x false)"
	    "        (true true)))")
       nil)
(test* "(not (eq 'a 'a))"
       false)
(test* "(not (eq 'a 'b))"
       true)

(println "\n(DEFUN APPEND)")
(test* (str "(defun append (x y)"
	    "  (cond ((null x) y)"
	    "        (true (cons (car x) (append (cdr x) y)))))")
       nil)
(test* "(append '(a b) '(c d))"
       ["a" "b" "c" "d"])
(test* "(append '() '(c d))"
       ["c" "d"])

(println "\n(DEFUN PAIR)")
(test* (str "(defun pair (x y)"
	    "  (cond ((and (null x) (null y)) '())"
	    "        ((and (not (atom x)) (not (atom y)))"
	    "         (cons (cons (car x) (cons (car y) '()))"
	    "               (pair (cdr x) (cdr y))))))")
       nil)
(test* "(pair '(x y z) '(a b c))"
       [["x" "a"] ["y" "b"] ["z" "c"]])

(println "\n(DEFUN ASSOC)")
(test* (str "(defun assoc (x y)"
	    "  (cond ((eq (caar y) x) (cadar y))"
	    "        (true (assoc x (cdr y)))))")
       nil)
(test* "(assoc 'x '((x a) (y b)))"
       "a")
(test* "(assoc 'x '((x new) (x a) (y b)))"
       "new")

(println "\n" (map first @env))
