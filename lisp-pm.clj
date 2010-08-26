(ns lisp-pm)

(defn _atom [x]
  (cond
   (keyword? x) true
   (true? x) true
   (false? x) true
   (empty? x) true
   :else false))

(defn _car [x] (first x))
(defn _cdr [x] (rest x))
(defn _caar [x] (_car (_car x)))
(defn _cadr [x] (_car (_cdr x)))
(defn _cdar [x] (_cdr (_car x)))
(defn _cddr [x] (_cdr (_cdr x)))
(defn _cadar [x] (_car (_cdr (_car x))))
(defn _caddr [x] (_car (_cdr (_cdr x))))

(defn _null [x] (empty? x))
(defn _pairlis [x y a] (concat (map list x y) a))
(defn _assoc [x a] (first (filter #(= x (first %)) a)))

(declare _apply _eval _evcon _evlis)

(defn _apply [f x a]
  (cond
   (_atom f) (cond
	       (= f :car) (_caar x)
	       (= f :cdr) (_cdar x)
	       (= f :cons) (cons (_car x) (_cadr x))
	       (= f :atom) (_atom (_car x))
	       (= f :eq) (= (_car x) (_cadr x))
	       :else (_apply (_eval f a) x a))
   (= (_car f) :lambda) (_eval (_caddr f) (_pairlis (_cadr f) x a))
   (= (_car f) :label) (_apply (_caddr f)
			       x
			       (cons (cons (_cadr f) (_cddr f)) a)))) ;;!!

(defn _eval [e a]
  (cond
   (_atom e) (_cadr (_assoc e a)) ;;!!
   (_atom (_car e))
   (cond
    (= (_car e) :quote) (_cadr e)
    (= (_car e) :cond) (_evcon (_cdr e) a)
    :else (_apply (_car e) (_evlis (_cdr e) a) a))
   :else (_apply (_car e) (_evlis (_cdr e) a) a)))

(defn _evcon [c a]
  (cond
   (_eval (_caar c) a) (_eval (_cadar c) a)
   :else (_evcon (_cdr c) a)))

(defn _evlis [m a]
  (cond
   (_null m) []
   :else (cons (_eval (_car m) a)
	       (_evlis (_cdr m) a))))

(def env [[true true] [false false] [[] false]])
