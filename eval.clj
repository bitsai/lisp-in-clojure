(ns eval)

;; helpers
(defn atom* [x]
  (cond (string? x) "t"
        (empty? x) "t"
        :else '()))
(defn eq [x y]
  (if (= x y) "t" '()))

(defn caar [x] (first (first x)))
(defn cadr [x] (first (rest x)))
(defn cadar [x] (first (rest (first x))))
(defn caddr [x] (first (rest (rest x))))
(defn caddar [x] (first (rest (rest (first x)))))
(defn cadddr [x] (first (rest (rest (rest x)))))
(defn pair [x y] (map list x y))
(defn assoc* [x [[a b :as y] & ys]]
  (cond (nil? y) (throw (Exception. (str "'" x "' not defined!")))
        (= x a) b
        :else (recur x ys)))

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
  (cond (= "t" (atom* e))
        (assoc* e @a)
        (= "t" (atom* (first e)))
        (case (first e)
              "quote" (cadr e)
              "atom"  (atom* (eval* (cadr e) a))
              "eq"    (eq    (eval* (cadr e) a)
                             (eval* (caddr e) a))
              "car"   (first (eval* (cadr e) a))
              "cdr"   (rest  (eval* (cadr e) a))
              "cons"  (cons  (eval* (cadr e) a)
                             (eval* (caddr e) a))
              "cond"  (evcon (rest e) a)
              "defun" (defun e a)
              (eval* (cons (assoc* (first e) @a)
                           (rest e))
                     a))
        (= (caar e) "label")
        (eval* (cons (caddar e) (rest e))
               (atom (cons (list (cadar e) (first e))
                           @a)))
        (= (caar e) "lambda")
        (eval* (caddar e)
               (atom (concat (pair (cadar e) (evlis (rest e) a))
                             @a)))))

(defn evcon [c a]
  (if (= "t" (eval* (caar c) a))
    (eval* (cadar c) a)
    (evcon (rest c) a)))

(defn evlis [m a]
  (if (empty? m)
    '()
    (cons (eval* (first m) a)
          (evlis (rest m) a))))
