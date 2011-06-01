(ns eval)

(defn atom* [x]
  (cond (string? x) "t"
        (empty? x) "t"
        :else '()))

(defn eq [x y]
  (if (= x y) "t" '()))

(defn cadar [x] (first (rest (first x))))

(defn caddr [x] (first (rest (rest x))))

(defn caddar [x] (first (rest (rest (first x)))))

(defn pair [x y] (map list x y))

(defn assoc* [x [[a b :as y] & ys]]
  (cond (nil? y) (throw (Exception. (str "'" x "' not defined!")))
        (= x a) b
        :else (recur x ys)))

(defn defun [[_ name args body] a]
  (swap! a conj [name ["label" name ["lambda" args body]]])
  (str "'" name "' defined!"))

(declare eval* evcon evlis)

(defn eval* [e a]
  (cond (= "t" (atom* e))
        (assoc* e @a)
        (= "t" (atom* (first e)))
        (case (first e)
              "quote" (second e)
              "atom"  (atom* (eval* (second e) a))
              "eq"    (eq    (eval* (second e) a)
                             (eval* (caddr e) a))
              "car"   (first (eval* (second e) a))
              "cdr"   (rest  (eval* (second e) a))
              "cons"  (cons  (eval* (second e) a)
                             (eval* (caddr e) a))
              "cond"  (evcon (rest e) a)
              "defun" (defun e a)
              (eval* (cons (assoc* (first e) @a)
                           (rest e))
                     a))
        (= (ffirst e) "label")
        (eval* (cons (caddar e) (rest e))
               (atom (cons (list (cadar e) (first e))
                           @a)))
        (= (ffirst e) "lambda")
        (eval* (caddar e)
               (atom (concat (pair (cadar e) (evlis (rest e) a))
                             @a)))))

(defn evcon [c a]
  (if (= "t" (eval* (ffirst c) a))
    (eval* (cadar c) a)
    (evcon (rest c) a)))

(defn evlis [m a]
  (if (empty? m)
    '()
    (cons (eval* (first m) a)
          (evlis (rest m) a))))
