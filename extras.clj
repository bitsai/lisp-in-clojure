(ns extras
  (:use [eval :only (env eval*)])
  (:use [reader :only (read*)]))

(defn add-fn [exp] (eval* (read* exp) env))

(add-fn (str "(defun subst (x y z)"
	     "  (cond ((atom z)"
	     "         (cond ((eq z y) x)"
	     "               ('t z)))"
	     "        ('t (cons (subst x y (car z))"
	     "                  (subst x y (cdr z))))))"))

(add-fn "(defun caar (x) (car (car x)))")

(add-fn "(defun cadr (x) (car (cdr x)))")

(add-fn "(defun cadar (x) (car (cdr (car x))))")

(add-fn "(defun caddr (x) (car (cdr (cdr x))))")

(add-fn "(defun caddar (x) (car (cdr (cdr (car x)))))")

(add-fn "(defun tuple (x y) (cons x (cons y '())))")

(add-fn "(defun null (x) (eq x '()))")

(add-fn (str "(defun and (x y)"
	     "  (cond (x (cond (y 't) ('t 'f)))"
	     "        ('t 'f)))"))

(add-fn (str "(defun not (x)"
	     "  (cond (x 'f)"
	     "        ('t 't)))"))

(add-fn (str "(defun append (x y)"
	     "  (cond ((null x) y)"
	     "        ('t (cons (car x) (append (cdr x) y)))))"))

(add-fn (str "(defun pair (x y)"
	     "  (cond ((and (null x) (null y)) '())"
	     "        ((and (not (atom x)) (not (atom y)))"
	     "         (cons (tuple (car x) (car y))"
	     "               (pair (cdr x) (cdr y))))))"))

(add-fn (str "(defun assoc (x y)"
	     "  (cond ((eq (caar y) x) (cadar y))"
	     "        ('t (assoc x (cdr y)))))"))

(add-fn (str "(defun eval (e a)"
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
	     "           (cons (tuple (cadar e) (car e)) a)))"
	     "    ((eq (caar e) 'lambda)"
	     "     (eval (caddar e)"
	     "           (append (pair (cadar e) (evlis (cdr e) a))"
	     "                   a)))))"))

(add-fn (str "(defun evcon (c a)"
	     "  (cond ((eval (caar c) a) (eval (cadar c) a))"
	     "        ('t (evcon (cdr c) a))))"))

(add-fn (str "(defun evlis (m a)"
	     "  (cond ((null m) '())"
	     "        ('t (cons (eval  (car m) a)"
	     "                  (evlis (cdr m) a)))))"))
