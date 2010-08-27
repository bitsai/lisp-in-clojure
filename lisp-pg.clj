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
(defn _list [x y] (_cons x (_cons y [])))
(defn _null [x] (_eq x []))
(defn _and [x y]
  (cond
   x (cond
      y true
      :else false)
   :else false))
(defn _not [x]
  (cond
   x false
   :else true))
(defn _append [x y]
  (cond
   (_null x) y
   :else (_cons (_car x) (_append (_cdr x) y))))
(defn _pair [x y]
  (cond
   (_and (_null x) (_null y)) []
   (_and (_not (_atom x)) (_not (_atom y))) (_cons (_list (_car x) (_car y))
						   (_pair (_cdr x) (_cdr y)))))
(defn _assoc [x y]
  (cond
   (_eq (_caar y) x) (_cadar y)
   :else (_assoc x (_cdr y))))

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
