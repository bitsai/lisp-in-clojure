(declare _apply _eval _pairlis _assoc _evcon _evlis)

(defn _atom? [x]
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

(defn _apply [f x a]
  (cond
   (_atom? f) (cond
	       (= f :car) (_caar x)
	       (= f :cdr) (_cdar x)
	       (= f :cons) (cons (_car x) (_cadr x))
	       (= f :atom) (_atom? (_car x))
	       (= f :eq) (= (_car x) (_cadr x))
	       :else (_apply (_eval f a) x a))
   (= (_car f) :lambda) (_eval (_caddr f) (_pairlis (_cadr f) x a))
   (= (_car f) :label) (_apply (_caddr f)
			       x
			       (cons (cons (_cadr f) (_cddr f)) a)))) ;;!!

(defn _eval [e a]
  (cond
   (_atom? e) (_cadr (_assoc e a)) ;;!!
   (_atom? (_car e))
   (cond
    (= (_car e) :quote) (_cadr e)
    (= (_car e) :cond) (_evcon (_cdr e) a)
    :else (_apply (_car e) (_evlis (_cdr e) a) a))
   :else (_apply (_car e) (_evlis (_cdr e) a) a)))

(defn _pairlis [x y a] (concat (map vector x y) a))

(defn _assoc [x a] (first (filter #(= x (first %)) a)))

(defn _evcon [c a]
  (if (_eval (_caar c) a) (_eval (_cadar c) a)
      (_evcon (_cdr c) a)))

(defn _evlis [m a]
  (if (empty? m) []
      (cons (_eval (_car m) a)
	    (_evlis (_cdr m) a))))

(def env [[true true] [false false] [[] false]])

;; quote
(_eval [:quote :a] env)
(_eval [:quote [:a :b :c]] env)
;; atom
(_eval [:atom [:quote :a]] env)
(_eval [:atom [:quote [:a :b :c]]] env)
(_eval [:atom [:quote []]] env)
(_eval [:atom [:atom [:quote []]]] env)
(_eval [:atom [:quote [:atom [:quote []]]]] env)
;; eq
(_eval [:eq [:quote :a] [:quote :a]] env)
(_eval [:eq [:quote :a] [:quote :b]] env)
(_eval [:eq [:quote []] [:quote []]] env)
;; car
(_eval [:car [:quote [:a :b :c]]] env)
;; cdr
(_eval [:cdr [:quote [:a :b :c]]] env)
;; cons
(_eval [:cons [:quote :a] [:quote [:b :c]]] env)
(_eval [:cons [:quote :a] [:cons [:quote :b] [:cons [:quote :c] [:quote []]]]] env)
(_eval [:car [:cons [:quote :a] [:quote [:b :c]]]] env)
(_eval [:cdr [:cons [:quote :a] [:quote [:b :c]]]] env)
;; cond
(_eval [:cond
	[[:eq [:quote :a] [:quote :b]] [:quote :first]]
	[[:atom [:quote :a]] [:quote :second]]] env)
;; lambda
(_eval [[:lambda [:x] [:cons :x [:quote [:b]]]]
	[:quote :a]] env)
(_eval [[:lambda [:x :y] [:cons :x [:cdr :y]]]
	[:quote :z]
	[:quote [:a :b :c]]] env)
(_eval [[:lambda [:f] [:f [:quote [:b :c]]]]
	[:quote [:lambda [:x] [:cons [:quote :a] :x]]]] env)
;; label
(_eval [[:label :subst [:lambda [:x :y :z]
			[:cond
			 [[:atom :z]
			  [:cond
			   [[:eq :z :y] :x]
			   [true :z]]]
			 [true [:cons
				[:subst :x :y [:car :z]]
				[:subst :x :y [:cdr :z]]]]]]]
	[:quote :m]
	[:quote :b]
	[:quote [:a :b [:a :b :c] :d]]] env)
;; eval
(_eval :x
       (concat [[:x :a] [:y :b]] env))
(_eval [:eq [:quote :a] [:quote :a]]
       env)
(_eval [:cons :x [:quote [:b :c]]]
       (concat [[:x :a] [:y :b]] env))
(_eval [:cond
	[[:atom :x] [:quote :atom]]
	[true [:quote :list]]]
       (concat [[:x [:quote [:a :b]]]] env))
(_eval [:f [:quote [:b :c]]]
       (concat [[:f [:lambda [:x] [:cons [:quote :a] :x]]]] env))
(_eval [[:label :first [:lambda [:x]
			[:cond
			 [[:atom :x] :x]
			 [true [:first [:car :x]]]]]]
	:y]
       (concat [[:y [[:a :b] [:c :d]]]] env))
(_eval [[:lambda [:x :y] [:cons :x [:cdr :y]]]
	[:quote :a]
	[:quote [:b :c :d]]]
       env)
