(ns aloop.core)

(defmacro !!!
  "Triple caution or attention macro"
  [& forms]
  `(comment ~@forms))

(defmacro example
  [& forms]
  `(comment ~@forms))

(defmacro if-
  "Takes `predicate p` of at least one argument and a list of `forms`.
  If predicate is true - returns its first argument, otherwise
  executes `else` forms in the implicit `do`"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p arg (cons 'do else))))

(example
  (if- (keyword? 'bar)
       :foo)
  :=> :foo

  (if- (keyword? :bar)
       :foo)
  :=> :bar)

(defmacro if->
  "Takes `predicate p` of at least one argument and a list of `forms`.
  If predicate is true - returns its first argument, otherwise
  threads `else` forms with `ps first arg`"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p arg `(-> ~arg ~@else))))

(example
  (if-> (keyword? 'bar)
        name
        clojure.string/upper-case
        keyword)
  :=> :BAR

  (if-> (keyword? :bar)
        name
        clojure.string/upper-case
        keyword)
  :=> :bar)

(defmacro if->>
  "Takes `predicate p` of at least one argument and a list of `forms`.
  If predicate is true - returns its first argument, otherwise
  threads last `else` forms with `ps first arg`"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p arg `(->> ~arg ~@else))))

(example
  (if->> (keyword? 'bar)
         name
         (clojure.string/replace "abc" #"c")
         keyword)
  :=> :abbar

  (if->> (keyword? :bar)
         name
         (clojure.string/replace "abc" #"c")
         keyword)
  :=> :bar)

(defmacro if+
  "Takes `predicate p` of at least one argument and a list of `forms`.
   If predicate is false - returns its first argument, otherwise
   executes `then` forms in the implicit `do`"
  [p & then]
  (if-let [arg (second p)]
    (list 'if p (cons 'do then) arg)))

(example
  (if+ (keyword? :bar)
       (name :bar))
  :=> "bar"

  (if+ (keyword? 'bar)
       :foo)
  :=> 'bar)

(defmacro if+>
  "Takes `predicate p` of at least one argument and a list of `forms`.
  If predicate is false - returns its first argument, otherwise
  threads `then` forms with `ps first arg`"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p `(-> ~arg ~@else) arg)))

(example
  (if+> (keyword? :bar)
        name
        clojure.string/upper-case
        symbol)
  :=> 'BAR

  (if+> (keyword? 'bar)
        name
        clojure.string/upper-case
        symbol)
  :=> 'bar)

(defmacro if+>>
  "Takes `predicate p` of at least one argument and a list of `forms`.
  If predicate is false - returns its first argument, otherwise
  threads last `then` forms with `ps first arg`"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p `(->> ~arg ~@else) arg)))

(example
  (if+>> (keyword? :bar)
         name
         (clojure.string/replace "abc" #"c")
         keyword)
  :=> :abbar

  (if+>> (keyword? 'bar)
         name
         (clojure.string/replace "abc" #"c")
         keyword)
  :=> 'bar)

(defmacro with->
  "Takes `x` and a list of `forms` and executes `forms` in implicit `do` providing
  `x` as first argument to every `form` in `forms`. Unlike `->` doesn't thread and
  executes in a procedure manner"
  [x & forms]
  (loop [d (list 'do), forms forms]
    (if forms
      (let [form (first forms)
            with (if (seq? form)
                   (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                   (list form x))]
        (recur (concat d (list with)) (next forms)))
      d)))

(example
  (with-> 722
          println
          inc)
  > 722
  :=> 723)

(defmacro with->>
  "Takes `x` and a list of `forms` and executes `forms` in implicit `do` providing
  `x` as last argument to every `form` in `forms`. Unlike `->>` doesn't thread and
  executes in a procedure manner."
  [x & forms]
  (loop [d (list 'do), forms forms]
    (if forms
      (let [form (first forms)
            with (if (seq? form)
                   (with-meta `(~(first form) ~@(next form) ~x) (meta form))
                   (list form x))]
        (recur (concat d (list with)) (next forms)))
      d)))

(example
  (with->> 722
           println
           inc)
  > 722
  :=> 723)
