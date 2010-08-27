(ns lisp-pm)

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
(defn _cdar [x] (_cdr (_car x)))
(defn _cddr [x] (_cdr (_cdr x)))
(defn _cadar [x] (_car (_cdr (_car x))))
(defn _caddr [x] (_car (_cdr (_cdr x))))
(defn _null [x] (_eq x []))
(defn _pairlis [x y a]
  (cond
   (_null x) a
   ;; :else (_cons (_cons (_car x) (_car y))
   :else (_cons (_cons (_car x) (_cons (_car y) []))
		(_pairlis (_cdr x) (_cdr y) a))))
(defn _assoc [x a]
  (cond
   (_eq (_caar a) x) (_car a)
   :else (_assoc x (_cdr a))))

;; interpreter
(declare _apply _eval _evcon _evlis)

(defn _apply [f x a]
  (cond
   (_atom f) (cond
	      (_eq f :car) (_caar x)
	      (_eq f :cdr) (_cdar x)
	      (_eq f :cons) (_cons (_car x) (_cadr x))
	      (_eq f :atom) (_atom (_car x))
	      (_eq f :eq) (_eq (_car x) (_cadr x))
	      :else (_apply (_eval f a) x a))
   (_eq (_car f) :lambda) (_eval (_caddr f) (_pairlis (_cadr f) x a))
   (_eq (_car f) :label) (_apply (_caddr f)
				 x
				 ;; (_cons (_cons (_cadr f) (_caddr f)) a))))
				 (_cons (_cons (_cadr f) (_cddr f)) a))))

(defn _eval [e a]
  (cond
   ;; (_atom e) (_cdr (_assoc e a))
   (_atom e) (_cadr (_assoc e a))
   (_atom (_car e)) (cond
		     (_eq (_car e) :quote) (_cadr e)
		     (_eq (_car e) :cond) (_evcon (_cdr e) a)
		     :else (_apply (_car e) (_evlis (_cdr e) a) a))
   :else (_apply (_car e) (_evlis (_cdr e) a) a)))

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
