(ns lisp-pg)

;; primitives
(defn _atom [x]
  (cond
   (keyword? x) true
   (true? x) true
   (false? x) true
   (empty? x) true
   :else false))
(defn _eq [x y] (= x y))
(defn _car [x] (first x))
(defn _cdr [x] (rest x))
(defn _cons [x y] (cons x y))

;; derived
(defn _caar [x] (_car (_car x)))
(defn _cadr [x] (_car (_cdr x)))
(defn _cadar [x] (_car (_cdr (_car x))))
(defn _caddr [x] (_car (_cdr (_cdr x))))
(defn _caddar [x] (_car (_cdr (_cdr (_car x)))))
(defn _list [& es] (vec es))
(defn _null [x] (empty? x))
(defn _append [x y] (concat x y))
(defn _pair [x y] (map vector x y))
(defn _assoc [x y]
  (let [matching-pair (first (filter #(= x (first %)) y))]
    (second matching-pair)))

;; interpreter
(declare _eval _evcon _evlis)

(defn _eval [e a]
  (cond
   (_atom e) (_assoc e a)
   (_atom (_car e)) (cond
		     (_eq (_car e) :quote) (_cadr e)
		     (_eq (_car e) :atom) (_atom (_eval (_cadr e) a))
		     (_eq (_car e) :eq) (_eq (_eval (_cadr e) a)
					     (_eval (_caddr e) a))
		     (_eq (_car e) :car) (_car (_eval (_cadr e) a))
		     (_eq (_car e) :cdr) (_cdr (_eval (_cadr e) a))
		     (_eq (_car e) :cons) (_cons (_eval (_cadr e) a)
						 (_eval (_caddr e) a))
		     (_eq (_car e) :cond) (_evcon (_cdr e) a)
		     :else (_eval (_cons (_assoc (_car e) a)
					 (_cdr e))
				  a))
   (_eq (_caar e) :label) (_eval (_cons (_caddar e) (_cdr e))
				 (_cons (_list (_cadar e) (_car e)) a))
   (_eq (_caar e) :lambda) (_eval (_caddar e)
				  (_append (_pair (_cadar e) (_evlis (_cdr e) a))
					   a))))

(defn _evcon [c a]
  (cond
   (_eval (_caar c) a) (_eval (_cadar c) a)
   :else (_evcon (_cdr c) a)))

(defn _evlis [m a]
  (cond
   (_null m) []
   :else (_cons (_eval (_car m) a)
		(_evlis (_cdr m) a))))

;; environment
(def env [[true true]
	  [false false]
	  [[] false]])
