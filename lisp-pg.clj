(ns lisp-pg)

(declare _eval _evcon _evlis)

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
(defn _cadar [x] (_car (_cdr (_car x))))
(defn _caddr [x] (_car (_cdr (_cdr x))))

(defn _assoc [x y] (second (first (filter #(= x (first %)) y))))

(defn _eval [e a]
  (cond
   (_atom e) (_assoc e a)
   (_atom (_car e))
   (cond
    (= (_car e) :quote) (_cadr e)
    (= (_car e) :atom) (_atom (_eval (_cadr e) a))
    (= (_car e) :eq) (= (_eval (_cadr e) a)
			(_eval (_caddr e) a))
    (= (_car e) :car) (_car (_eval (_cadr e) a))
    (= (_car e) :cdr) (_cdr (_eval (_cadr e) a))
    (= (_car e) :cons) (cons (_eval (_cadr e) a)
			     (_eval (_caddr e) a))
    (= (_car e) :cond) (_evcon (_cdr e) a))))

(defn _evcon [c a]
  (if (_eval (_caar c) a) (_eval (_cadar c) a)
      (_evcon (_cdr c) a)))

(def env [[true true] [false false] [[] false]])
