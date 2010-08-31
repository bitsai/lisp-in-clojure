(ns tests
  (:use [interpreter :only (_eval env)])
  (:use [reader :only (_read)]))

(defn _test
  ([exp-str answer] (_test exp-str "" answer))
  ([exp-str env-str answer]
     (let [test-exp (_read exp-str)
	   test-env (_read env-str)
	   result (_eval test-exp (concat test-env env))]
       (if (= result answer) (println "OK")
	   (println "FAIL")))))

(println "\n(QUOTE)")
(_test "(quote a)"
       "a")
;; needs support for '
;; (_test "'a",
;;        "a")
(_test "(quote (a b c))"
       '("a" "b" "c"))

(println "\n(ATOM)")
(_test "(atom (quote a))"
       true)
(_test "(atom (quote (a b c)))"
       false)
(_test "(atom (quote ()))"
       true)
(_test "(atom (atom (quote a)))"
       true)
(_test "(atom (quote (atom (quote a))))"
       false)

(println "\n(EQ)")
(_test "(eq (quote a) (quote a))"
       true)
(_test "(eq (quote a) (quote b))"
       false)
(_test "(eq (quote ()) (quote ()))"
       true)

(println "\n(CAR)")
(_test "(car (quote (a b c)))"
       "a")

(println "\n(CDR)")
(_test "(cdr (quote (a b c)))"
       '("b" "c"))

(println "\n(CONS)")
(_test "(cons (quote a) (quote (b c)))"
       '("a" "b" "c"))
(_test "(cons (quote a) (cons (quote b) (cons (quote c) (quote ()))))"
       '("a" "b" "c"))
(_test "(car (cons (quote a) (quote (b c))))"
       "a")
(_test "(cdr (cons (quote a) (quote (b c))))"
       '("b" "c"))

(println "\n(COND)")
(_test (str "(cond ((eq (quote a) (quote b)) (quote first))"
	    "      ((atom (quote a)) (quote second)))")
       "second")

(println "\n(LAMBDA)")
(_test "((lambda (x) (cons x (quote (b)))) (quote a))"
       '("a" "b"))
(_test (str "((lambda (x y) (cons x (cdr y)))"
	    " (quote z)"
	    " (quote (a b c)))")
       '("z" "b" "c"))
(_test (str "((lambda (f) (f (quote (b c))))"
	    " (quote (lambda (x) (cons (quote a) x))))")
       '("a" "b" "c"))

(println "\n(LABEL)")
(_test "(subst (quote m) (quote b) (quote (a b (a b c) d)))"
       (str "((subst (label subst (lambda (x y z)"
	    "                       (cond ((atom z)"
	    "                              (cond ((eq z y) x)"
	    "                                    (true z)))"
	    "                             (true (cons (subst x y (car z))"
	    "                                         (subst x y (cdr z)))))))))")
       '("a" "m" ("a" "m" "c") "d"))

(println "\n(EVAL)")
(_test "x"
       "((x a) (y b))"
       "a")
(_test "(eq (quote a) (quote a))"
       true)
(_test "(cons x (quote (b c)))"
       "((x a) (y b))"
       '("a" "b" "c"))
(_test (str "(cond ((atom x) (quote atom))"
	    "      (true (quote list)))")
       "((x (quote (a b))))"
       "list")
(_test "(f (quote (b c)))"
       "((f (lambda (x) (cons (quote a) x))))"
       '("a" "b" "c"))
(_test (str "((label firstatom (lambda (x)"
	    "                    (cond ((atom x) x)"
	    "                          (true (firstatom (car x))))))"
	    " y)")
       "((y ((a b) (c d))))"
       "a")
(_test (str "((lambda (x y) (cons x (cdr y)))"
	    " (quote a)"
	    " (quote (b c d)))")
       '("a" "c" "d"))
