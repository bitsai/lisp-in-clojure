(ns eval)

;; primitive
(defn atom* [x]
  (cond
   (string? x) "t"
   (empty? x) "t"
   :else "f"))
(defn eq [x y]
  (if (= x y) "t"
      "f"))
(defn car [x] (first x))
(defn cdr [x] (next x))

;; derived
(defn caar [x] (ffirst x))
(defn cadr [x] (fnext x))
(defn cadar [x] (fnext (first x)))
(defn caddr [x] (fnext (next x)))
(defn caddar [x] (fnext (nfirst x)))
(defn cadddr [x] (fnext (nnext x)))
(defn pair [x y] (map list x y))
(defn assoc* [x y]
  (let [matches? #(= x (first %))
	match (first (filter matches? @y))]
    (if (nil? match) (throw (Exception. (str x " not defined!")))
	(second match))))

;; eval and friends
(declare eval* evcon evlis defun)

(defn eval* [e a]
  (try
    (cond
     (= "t" (atom* e)) (assoc* e a)
     (= "t" (atom* (car e)))
     (cond
      (= (car e) "quote") (cadr e)
      (= (car e) "atom")  (atom* (eval* (cadr e) a))
      (= (car e) "eq")    (eq    (eval* (cadr e) a)
				 (eval* (caddr e) a))
      (= (car e) "car")   (car   (eval* (cadr e) a))
      (= (car e) "cdr")   (cdr   (eval* (cadr e) a))
      (= (car e) "cons")  (cons  (eval* (cadr e) a)
				 (eval* (caddr e) a)) 
      (= (car e) "cond")  (evcon (cdr e) a)
      (= (car e) "defun") (defun e a)
      :else (eval* (cons (assoc* (car e) a)
			 (cdr e))
		   a))
     (= (caar e) "label")
     (eval* (cons (caddar e) (cdr e))
	    (ref (cons (list (cadar e) (car e))
		       @a)))
     (= (caar e) "lambda")
     (eval* (caddar e)
	    (ref (concat (pair (cadar e) (evlis (cdr e) a))
			 @a))))
    (catch Exception ex (.getMessage ex))))

(defn evcon [c a]
  (cond (= "t" (eval* (caar c) a)) (eval* (cadar c) a)
	:else (evcon (cdr c) a)))

(defn evlis [m a]
  (cond (empty? m) nil
	:else (cons (eval* (car m) a)
		    (evlis (cdr m) a))))

(defn defun [e a]
  (let [name (cadr e)
	args (caddr e)
	body (cadddr e)
	label-fn ["label" name ["lambda" args body]]]
    (dosync (alter a conj [name label-fn]))
    nil))

;; environment
(def env (ref []))

;; defun additional functions
(load-file "extras.clj")
