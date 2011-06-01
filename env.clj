(ns env
  (:use [eval :only (eval*)])
  (:use [reader :only (read*)]))

(def defun-exps
  [(str "(defun subst (x y z)"
	"  (cond ((atom z)"
	"         (cond ((eq z y) x)"
	"               ('t z)))"
	"        ('t (cons (subst x y (car z))"
	"                  (subst x y (cdr z))))))")

   "(defun caar (x) (car (car x)))"

   "(defun cadr (x) (car (cdr x)))"

   "(defun cdar (x) (cdr (car x)))"

   "(defun cadar (x) (car (cdr (car x))))"

   "(defun caddr (x) (car (cdr (cdr x))))"

   "(defun caddar (x) (car (cdr (cdr (car x)))))"

   "(defun list (x y) (cons x (cons y '())))"

   (str "(defun null (x)"
	"  (eq x '()))")

   (str "(defun and (x y)"
	"  (cond (x (cond (y 't) ('t '())))"
	"        ('t '())))")

   (str "(defun not (x)"
	"  (cond (x '())"
	"        ('t 't)))")

   (str "(defun append (x y)"
	"  (cond ((null x) y)"
	"        ('t (cons (car x) (append (cdr x) y)))))")

   (str "(defun pair (x y)"
	"  (cond ((and (null x) (null y)) '())"
	"        ((and (not (atom x)) (not (atom y)))"
	"         (cons (list (car x) (car y))"
	"               (pair (cdr x) (cdr y))))))")

   (str "(defun assoc (x y)"
	"  (cond ((eq (caar y) x) (cadar y))"
	"        ('t (assoc x (cdr y)))))")

   (str "(defun eval (e a)"
	"  (cond"
	"    ((atom e) (assoc e a))"
	"    ((atom (car e))"
	"     (cond"
	"       ((eq (car e) 'quote) (cadr e))"
	"       ((eq (car e) 'atom)  (atom  (eval (cadr e) a)))"
	"       ((eq (car e) 'eq)    (eq    (eval (cadr e) a)"
	"                                   (eval (caddr e) a)))"
	"       ((eq (car e) 'car)   (car   (eval (cadr e) a)))"
	"       ((eq (car e) 'cdr)   (cdr   (eval (cadr e) a)))"
	"       ((eq (car e) 'cons)  (cons  (eval (cadr e) a)"
	"                                   (eval (caddr e) a)))"
	"       ((eq (car e) 'cond)  (evcon (cdr e) a))"
	"       ('t (eval (cons (assoc (car e) a)"
	"                       (cdr e))"
	"                 a))))"
	"    ((eq (caar e) 'label)"
	"     (eval (cons (caddar e) (cdr e))"
	"           (cons (list (cadar e) (car e)) a)))"
	"    ((eq (caar e) 'lambda)"
	"     (eval (caddar e)"
	"           (append (pair (cadar e) (evlis (cdr e) a))"
	"                   a)))))")

   (str "(defun evcon (c a)"
	"  (cond ((eval (caar c) a)"
	"         (eval (cadar c) a))"
	"        ('t (evcon (cdr c) a))))")

   (str "(defun evlis (m a)"
	"  (cond ((null m) '())"
	"        ('t (cons (eval  (car m) a)"
	"                  (evlis (cdr m) a)))))")])

(defn make-env []
  (let [env (atom '())]
    (doseq [exp defun-exps]
      (eval* (read* exp) env))
    env))
