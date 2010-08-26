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
