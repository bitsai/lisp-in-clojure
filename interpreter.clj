(ns interpreter)

;; environment
(def env [["true" true]
	  ["false" false]])

;; primitive
(defn atom* [x]
  (cond
   (string? x) true
   (true? x) true
   (false? x) true
   (empty? x) true
   :else false))
(defn car* [x] (first x))
(defn cdr* [x] (next x))

;; derived
(defn caar* [x] (ffirst x))
(defn cadr* [x] (fnext x))
(defn cadar* [x] (fnext (first x)))
(defn caddr* [x] (fnext (next x)))
(defn caddar* [x] (fnext (nfirst x)))
(defn pair* [x y] (map list x y))
(defn assoc* [x y]
  (let [match (first (filter #(= x (first %)) y))]
    (if (nil? match) (throw (Exception. (str x " not defined!")))
	(second match))))

;; eval and friends
(declare eval* evcon* evlis*)

(defn eval* [e a]
  (cond
   (atom* e) (assoc* e a)
   (atom* (car* e)) (cond
		     (= (car* e) "quote") (cadr* e)
		     (= (car* e) "atom") (atom* (eval* (cadr* e) a))
		     (= (car* e) "eq") (= (eval* (cadr* e) a)
					  (eval* (caddr* e) a))
		     (= (car* e) "car") (car* (eval* (cadr* e) a))
		     (= (car* e) "cdr") (cdr* (eval* (cadr* e) a))
		     (= (car* e) "cons") (cons (eval* (cadr* e) a)
					       (eval* (caddr* e) a))
		     (= (car* e) "cond") (evcon* (cdr* e) a)
		     :else (eval* (cons (assoc* (car* e) a)
					(cdr* e))
				  a))
   (= (caar* e) "label") (eval* (cons (caddar* e) (cdr* e))
				(cons (list (cadar* e) (car* e)) a))
   (= (caar* e) "lambda") (eval* (caddar* e)
				 (concat (pair* (cadar* e) (evlis* (cdr* e) a))
					 a))))

(defn evcon* [c a]
  (cond
   (eval* (caar* c) a) (eval* (cadar* c) a)
   :else (evcon* (cdr* c) a)))

(defn evlis* [m a]
  (cond
   (empty? m) nil
   :else (cons (eval* (car* m) a)
	       (evlis* (cdr* m) a))))

;; exception-catching eval wrapper
(defn evaluate [e a]
  (try (eval* e a)
       (catch Exception ex (.getMessage ex))))
