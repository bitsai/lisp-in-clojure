(ns tests
  (:use [interpreter :only (env evaluate)])
  (:use [reader :only (parse)]))

(defn run
  ([exp-str answer] (run exp-str "" answer))
  ([exp-str env-str answer]
     (let [test-exp (parse exp-str)
	   test-env (parse env-str)
	   result (evaluate test-exp (concat test-env env))]
       (if (= result answer) (println "OK")
	   (println "FAIL")))))

(println "\n(QUOTE)")
(run "(quote a)"
     "a")
;; (run "'a",
;;        "a")
(run "(quote (a b c))"
     '("a" "b" "c"))

(println "\n(ATOM)")
(run "(atom (quote a))"
     true)
(run "(atom (quote (a b c)))"
     false)
(run "(atom (quote ()))"
     true)
(run "(atom (atom (quote a)))"
     true)
(run "(atom (quote (atom (quote a))))"
     false)

(println "\n(EQ)")
(run "(eq (quote a) (quote a))"
     true)
(run "(eq (quote a) (quote b))"
     false)
(run "(eq (quote ()) (quote ()))"
     true)

(println "\n(CAR)")
(run "(car (quote (a b c)))"
     "a")

(println "\n(CDR)")
(run "(cdr (quote (a b c)))"
     '("b" "c"))

(println "\n(CONS)")
(run "(cons (quote a) (quote (b c)))"
     '("a" "b" "c"))
(run "(cons (quote a) (cons (quote b) (cons (quote c) (quote ()))))"
     '("a" "b" "c"))
(run "(car (cons (quote a) (quote (b c))))"
     "a")
(run "(cdr (cons (quote a) (quote (b c))))"
     '("b" "c"))

(println "\n(COND)")
(run (str "(cond ((eq (quote a) (quote b)) (quote first))"
	  "      ((atom (quote a)) (quote second)))")
     "second")

(println "\n(LAMBDA)")
(run "((lambda (x) (cons x (quote (b)))) (quote a))"
     '("a" "b"))
(run (str "((lambda (x y) (cons x (cdr y)))"
	  " (quote z)"
	  " (quote (a b c)))")
     '("z" "b" "c"))
(run (str "((lambda (f) (f (quote (b c))))"
	  " (quote (lambda (x) (cons (quote a) x))))")
     '("a" "b" "c"))

(println "\n(LABEL)")
(run "(subst (quote m) (quote b) (quote (a b (a b c) d)))"
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
(run "(eq (quote a) (quote a))"
     true)
(run "(cons x (quote (b c)))"
     "((x a) (y b))"
     '("a" "b" "c"))
(run (str "(cond ((atom x) (quote atom))"
	  "      (true (quote list)))")
     "((x (quote (a b))))"
     "list")
(run "(f (quote (b c)))"
     "((f (lambda (x) (cons (quote a) x))))"
     '("a" "b" "c"))
(run (str "((label firstatom (lambda (x)"
	  "                    (cond ((atom x) x)"
	  "                          (true (firstatom (car x))))))"
	  " y)")
     "((y ((a b) (c d))))"
     "a")
(run (str "((lambda (x y) (cons x (cdr y)))"
	  " (quote a)"
	  " (quote (b c d)))")
     '("a" "c" "d"))
