(ns eval)

;; helpers
(defn atom* [x]
  (cond
   (string? x) "t"
   (empty? x) "t"
   :else "f"))
(defn eq [x y]
  (if (= x y)
    "t"
    "f"))

(defn caar [x] (first (first x)))
(defn cadr [x] (first (rest x)))
(defn cadar [x] (first (rest (first x))))
(defn caddr [x] (first (rest (rest x))))
(defn caddar [x] (first (rest (rest (first x)))))
(defn cadddr [x] (first (rest (rest (rest x)))))
(defn pair [x y] (map list x y))
(defn assoc* [x [[a b] & ys]]
  (cond
   (= a x) b
   (seq? ys) (recur x ys)
   :else (throw (Exception. (str "'" x "' not defined!")))))

(defn defun [e a]
  (let [name (cadr e)
	args (caddr e)
	body (cadddr e)
	new-pair [name ["label" name ["lambda" args body]]]]
    (swap! a conj new-pair)
    (str "'" name "' defined!")))

;; eval and friends
(declare eval* evcon evlis)

(defn eval* [e a]
  (try
    (cond
     (= "t" (atom* e)) (assoc* e @a)
     (= "t" (atom* (first e)))
     (cond
      (= (first e) "quote") (cadr e)
      (= (first e) "atom")  (atom* (eval* (cadr e) a))
      (= (first e) "eq")    (eq    (eval* (cadr e) a)
				   (eval* (caddr e) a))
      (= (first e) "car")   (first (eval* (cadr e) a))
      (= (first e) "cdr")   (rest  (eval* (cadr e) a))
      (= (first e) "cons")  (cons  (eval* (cadr e) a)
				   (eval* (caddr e) a))
      (= (first e) "cond")  (evcon (rest e) a)
      (= (first e) "defun") (defun e a)
      :else (eval* (cons (assoc* (first e) @a)
			 (rest e))
		   a))
     (= (caar e) "label")
     (eval* (cons (caddar e) (rest e))
	    (atom (cons (list (cadar e) (first e)) @a)))
     (= (caar e) "lambda")
     (eval* (caddar e)
	    (atom (concat (pair (cadar e) (evlis (rest e) a))
			  @a))))
    (catch Exception ex (.getMessage ex))))

(defn evcon [c a]
  (cond (= "t" (eval* (caar c) a)) (eval* (cadar c) a)
	:else (evcon (rest c) a)))

(defn evlis [m a]
  (cond (empty? m) nil
	:else (cons (eval* (first m) a)
		    (evlis (rest m) a))))
