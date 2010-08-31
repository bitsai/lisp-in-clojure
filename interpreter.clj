(ns interpreter)

;; primitive
(defn _atom [x]
  (cond
   (string? x) true
   (true? x) true
   (false? x) true
   (empty? x) true
   :else false))
(defn _car [x] (first x))
(defn _cdr [x] (next x))

;; derived
(defn _caar [x] (ffirst x))
(defn _cadr [x] (fnext x))
(defn _cadar [x] (fnext (first x)))
(defn _caddr [x] (fnext (next x)))
(defn _caddar [x] (fnext (nfirst x)))
(defn _pair [x y] (map list x y))
(defn _assoc [x y]
  (let [match (first (filter #(= x (first %)) y))]
    (second match)))

;; eval and friends
(declare _eval _evcon _evlis)

(defn _eval [e a]
  (cond
   (_atom e) (_assoc e a)
   (_atom (_car e)) (cond
		     (= (_car e) "quote") (_cadr e)
		     (= (_car e) "atom") (_atom (_eval (_cadr e) a))
		     (= (_car e) "eq") (= (_eval (_cadr e) a)
					  (_eval (_caddr e) a))
		     (= (_car e) "car") (_car (_eval (_cadr e) a))
		     (= (_car e) "cdr") (_cdr (_eval (_cadr e) a))
		     (= (_car e) "cons") (cons (_eval (_cadr e) a)
					       (_eval (_caddr e) a))
		     (= (_car e) "cond") (_evcon (_cdr e) a)
		     :else (_eval (cons (_assoc (_car e) a)
					(_cdr e))
				  a))
   (= (_caar e) "label") (_eval (cons (_caddar e) (_cdr e))
				(cons (list (_cadar e) (_car e)) a))
   (= (_caar e) "lambda") (_eval (_caddar e)
				 (concat (_pair (_cadar e) (_evlis (_cdr e) a))
					 a))))

(defn _evcon [c a]
  (cond
   (_eval (_caar c) a) (_eval (_cadar c) a)
   :else (_evcon (_cdr c) a)))

(defn _evlis [m a]
  (cond
   (empty? m) '()
   :else (cons (_eval (_car m) a)
	       (_evlis (_cdr m) a))))

;; environment
(def env [["true" true]
	  ["false" false]])
