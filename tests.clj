(ns tests
  (:use [interpreter :only (env evaluate)])
  (:use [reader :only (parse)]))

(defn run
  ([exp-str answer] (run exp-str "" answer))
  ([exp-str env-str answer]
     (let [test-exp (parse exp-str)
	   test-env (concat env (parse env-str))
	   result (evaluate test-exp test-env)]
       (if (= result answer) (println "OK")
	   (println "FAIL")))))

(println "\n(QUOTE)")
(run "(quote a)"
     "a")
(run "'a",
     "a")
(run "(quote (a b c))"
     '("a" "b" "c"))

(println "\n(ATOM)")
(run "(atom 'a)"
     true)
(run "(atom '(a b c))"
     false)
(run "(atom '())"
     true)
(run "(atom (atom 'a))"
     true)
(run "(atom '(atom 'a))"
     false)

(println "\n(EQ)")
(run "(eq 'a 'a)"
     true)
(run "(eq 'a 'b)"
     false)
(run "(eq '() '())"
     true)

(println "\n(CAR)")
(run "(car '(a b c))"
     "a")

(println "\n(CDR)")
(run "(cdr '(a b c))"
     '("b" "c"))

(println "\n(CONS)")
(run "(cons 'a '(b c))"
     '("a" "b" "c"))
(run "(cons 'a (cons 'b (cons 'c '())))"
     '("a" "b" "c"))
(run "(car (cons 'a '(b c)))"
     "a")
(run "(cdr (cons 'a '(b c)))"
     '("b" "c"))

(println "\n(COND)")
(run (str "(cond ((eq 'a 'b) 'first)"
	  "      ((atom 'a)  'second))")
     "second")

(println "\n(LAMBDA)")
(run "((lambda (x) (cons x '(b))) 'a)"
     '("a" "b"))
(run (str "((lambda (x y) (cons x (cdr y)))"
	  " 'z"
	  " '(a b c))")
     '("z" "b" "c"))
(run (str "((lambda (f) (f '(b c)))"
	  " '(lambda (x) (cons 'a x)))")
     '("a" "b" "c"))

(println "\n(LABEL)")
(run "(subst 'm 'b '(a b (a b c) d))"
     (str "((subst (label subst (lambda (x y z)"
	  "                       (cond ((atom z)"
	  "                              (cond ((eq z y) x)"
	  "                                    (true z)))"
	  "                             (true (cons (subst x y (car z))"
	  "                                         (subst x y (cdr z)))))))))")
     '("a" "m" ("a" "m" "c") "d"))

(println "\n(EVAL)")
(run "x"
     "((x a) (y b))"
     "a")
(run "(eq 'a 'a)"
     true)
(run "(cons x '(b c))"
     "((x a) (y b))"
     '("a" "b" "c"))
(run (str "(cond ((atom x) 'atom)"
	  "      (true 'list))")
     "((x '(a b)))"
     "list")
(run "(f '(b c))"
     "((f (lambda (x) (cons 'a x))))"
     '("a" "b" "c"))
(run (str "((label firstatom (lambda (x)"
	  "                    (cond ((atom x) x)"
	  "                          (true (firstatom (car x))))))"
	  " y)")
     "((y ((a b) (c d))))"
     "a")
(run (str "((lambda (x y) (cons x (cdr y)))"
	  " 'a"
	  " '(b c d))")
     '("a" "c" "d"))
