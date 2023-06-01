(ns aloop.core)

(declare
  block
  !!!
  example
  rotate
  add->>seq
  add->seq
  replace->>seq
  replace->seq
  if-
  if->
  if-|>
  if->>
  if-<|
  if+
  if+>
  if+|>
  if+>>
  if+<|
  with->
  with-|>
  with->>
  with-<|
  <->
  <->>
  <<->
  <<->>
  |>
  <|)

(defn- -natural->
  [x form]
  (with-meta `(~(first form) ~x ~@(next form)) (meta form)))

(defn- -natural->>
  [x form]
  (with-meta `(~(first form) ~@(next form) ~x) (meta form)))

(defmacro !!!
  "Triple caution or attention macro"
  [& forms]
  `(comment ~@forms))

(defmacro example
  [& forms]
  `(comment ~@forms))

(block "Index manipulations")

(defn rotate
  "Takes `index` and right border `c`. Envelops `index` around the length of `c`, if `index` is positive - envelopes
  clockwise, otherwise - counterclockwise. In other words, uses different approaches for indexing a collection of
  length c. For positive values of `index` - count a first element of collection as 0, second as 1 and so on.
  For negative values - last element of collection will be 0, second to last - 1."
  [c index]
  (if (neg? index)
    (recur c (+ c index))
    (if+ (>= index c)
         (recur c (mod index c)))))

(example
  (rotate 3 0) :=> 0
  (rotate 3 4) :=> 1
  (rotate 3 -1) :=> 2)

(block "Functions on seq")
(!!! "Following functions will use rotation on indices, take a look on:" #'rotate)

(defn add->>seq
  "Takes an arbitrary collection `where` and an element `what`. Adds `what` to the index `place` in `where`.
  If no `place` provided - adds `what` to the end of `where`. Negatively rotates index `place`, i.e. counts indices
  backwards, where last element is 0, second to last is 1 and so on."
  ([where what]
   (if+ (seq? where)
        (->> where
             reverse
             (cons what)
             reverse)))
  ([where what place]
   (add->seq where what (- (count where) place))))

(example
  (add->>seq '(1 2 3 4) 5) :=> '(1 2 3 4 5)
  (add->>seq '(1 2 3 4) 5 0) :=> '(1 2 3 4 5)
  (add->>seq '(1 2 3 4) 5 1) :=> '(1 2 3 5 4)
  (add->>seq '(1 2 3 4) 5 -1) :=> '(5 1 2 3 4))

(defn add->seq
  "Takes an arbitrary collection `where` and an element `what`. Adds `what` to the index `place` in `where`.
  If no `place` provided - adds `what` to the beginning of `where`. Positively rotates index `place`,
  i.e. counts indices forward, where first element is 0, second is 1 and so on."
  ([where what]
   (add->seq where what 0))
  ([where what place]
   (if+ (seq? where)
        (let [place (-> where count inc (rotate place))]
          (->> where
               (drop place)
               (concat (add->>seq (take place where) what)))))))

(example
  (add->seq '(1 2 3 4) 5) :=> '(5 1 2 3 4)
  (add->seq '(1 2 3 4) 5 3) :=> '(1 2 3 5 4)
  (add->seq '(1 2 3 4) 5 0) :=> '(5 1 2 3 4)
  (add->seq '(1 2 3 4) 5 -1) :=> '(1 2 3 4 5))

(defn replace->>seq
  "Takes an arbitrary collection `where` and an element `what`. Replaces `what` on the index `place` in `where`.
  If no `place` provided - changes nothing. Negatively rotates index `place`, i.e. counts indices backwards,
  where last element is 0, second to last is 1 and so on."
  ([where what]
   (if+ (seq? where)
        (if- (empty? where)
             (-> where
                 (drop-last)
                 (add->>seq what)))))
  ([where what place]
   (replace->seq where what (-> where count (- place 1)))))

(example
  (replace->>seq '(1 2 3) 4) :=> '(1 2 4)
  (replace->>seq '(1 2 3) 4 0) :=> '(1 2 4)
  (replace->>seq '(1 2 3) 4 1) :=> '(1 4 3)
  (replace->>seq '(1 2 3) 4 -1) :=> '(4 2 3))

(defn replace->seq
  "Takes an arbitrary collection `where` and an element `what`. Replaces `what` on the index `place` in `where`.
  If no `place` provided - changes nothing. Positively rotates index `place`, i.e. counts indices forward, where
  first element is 0, second is 1 and so on."
  ([where what]
   (replace->seq where what 0))
  ([where what place]
   (if+ (seq? where)
        (let [place (-> where count (rotate place))]
          (-> place
              (take where)
              (concat (drop (inc place) where))
              (add->seq what place))))))

(example
  (replace->seq '(1 2 3) 4) :=> '(1 2 4)
  (replace->seq '(1 2 3) 4 0) :=> '(4 2 3)
  (replace->seq '(1 2 3) 4 -1) :=> '(1 2 4))

(defn swap
  "Takes an arbitrary collection `where` and two indices: `left` and `right`. Swaps value on the `left` with the value
  on the `right`. Positively rotates both indices `place`, i.e. counts indices forward, where first element is 0, second
  is 1 and so on."
  [where left right]
  (if+ (seq? where)
       (if- (empty? where)
            (let [left-value (nth where (rotate (count where) left))
                  right-value (nth where (rotate (count where) right))]
              (-> where
                  (replace->seq right-value left)
                  (replace->seq left-value right))))))

(example
  (swap '(1 2 3 4 5) 0 0) :=> '(1 2 3 4 5)
  (swap '(1 2 3 4 5) 0 3) :=> '(4 2 3 1 5)
  (swap '(1 2 3 4 5) 0 -1) :=> '(5 2 3 4 1)
  (swap '(1 2 3 4 5) -1 -2) :=> '(1 2 3 5 4))

(block "`If` macros")

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

(defmacro if-|>
  "Same as `if->`, but uses `|>` instead of `->`"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p arg `(|> ~arg ~@else))))

(example
  (if-|> (keyword? 'bar)
         name
         clojure.string/upper-case
         keyword)
  :=> :BAR

  (if-|> (keyword? :bar)
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

(defmacro if-<|
  "Same as `if->>`, but uses `<|` instead of `->>`"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p arg `(->> ~arg ~@else))))

(example
  (if-<| (keyword? 'bar)
         name
         (clojure.string/replace "abc" #"c")
         keyword)
  :=> :abbar

  (if-<| (keyword? :bar)
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

(defmacro if+|>
  "Same as `if+>`, but uses `|>` instead of `->`"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p `(-> ~arg ~@else) arg)))

(example
  (if+|> (keyword? :bar)
         name
         clojure.string/upper-case
         symbol)
  :=> 'BAR

  (if+|> (keyword? 'bar)
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

(defmacro if+<|
  "Same as `if+>>`, but uses `<|` instead of `->>`"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p `(->> ~arg ~@else) arg)))

(example
  (if+<| (keyword? :bar)
         name
         (clojure.string/replace "abc" #"c")
         keyword)
  :=> :abbar

  (if+<| (keyword? 'bar)
         name
         (clojure.string/replace "abc" #"c")
         keyword)
  :=> 'bar)

(block "`With` macros")
(!!! "Using of this macros is an indication of either procedural logic, or debugging, use with caution")

(defmacro with->
  "Takes `x` and a list of `forms` and executes `forms` in implicit `do` providing
  `x` as first argument to every `form` in `forms`. Unlike `->` doesn't thread and
  executes in a procedure manner"
  [x & forms]
  (loop [d (list 'do), forms forms]
    (if forms
      (let [form (first forms)
            with (if (seq? form)
                   (-natural-> x form)
                   (list form x))]
        (recur (concat d (list with)) (next forms)))
      d)))

(example
  (with-> 722
          println
          inc)
  > 722
  :=> 723)

(defmacro with-|>
  "Same as `with->`, but uses `|>` instead of `->`"
  [x & forms]
  (loop [d (list 'do), forms forms]
    (if forms
      (let [form (first forms)
            with (if (seq? form)
                   (if `(get-in (meta (var ~(first form))) [::aloop ::before-thread?])
                     (let [[op form] form]
                       `(~op ~(-natural-> x form)))
                     (-natural-> x form))
                   (list form x))]
        (recur (concat d (list with)) (next forms)))
      d)))

(example
  (with-|> 722
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
                   (-natural->> x form)
                   (list form x))]
        (recur (concat d (list with)) (next forms)))
      d)))

(example
  (with->> 722
           println
           inc)
  > 722
  :=> 723)

(defmacro with-<|
  "Same as `with->>`, but uses `<|` instead of `->>`"
  [x & forms]
  (loop [d (list 'do), forms forms]
    (if forms
      (let [form (first forms)
            with (if (seq? form)
                   (if `(get-in (meta (var ~(first form))) [::aloop ::before-thread?])
                     (let [[op form] form]
                       `(~op ~(-natural->> x form)))
                     (-natural->> x form))]
        (recur (concat d (list with)) (next forms)))
      d)))

(example
  (with-<| 722
           println
           inc)
  > 722
  :=> 723)

(block "Following macros will alter order of function args")

(defmacro <->
  "Take a form of any args and switch places for first and second arg if any"
  {::aloop {::before-thread? true}}
  [form]
  (if+ (coll? form)
       (case (count form)
         1 `(~(first form))
         2 `(~(first form) ~(second form))
         `(~(first form) ~(nth form 2) ~(second form) ~@(drop 3 form)))))

(example
  (<-> (println :foo :boo :loo))
  > :boo :foo :loo
  :=> nil)

(defmacro <->>
  "Take a form of any args and switch places for last and second to last arg if any"
  {::aloop {::before-thread? true}}
  [form]
  (if+ (coll? form)
       (case (count form)
         1 `(~(first form))
         2 `(~(first form) ~(second form))
         (swap form -2 -1))))

(example
  (<->> (println :foo :boo :loo))
  > :foo :loo :boo
  :=> nil)

(defmacro <<->>
  "Take a form of any args and switch places for first and last arg if any"
  [form]
  {::aloop {::before-thread? true}}
  (if+ (coll? form)
       (case (count form)
         1 `(~(first form))
         2 `(~(first form) ~(second form))
         (swap form -1 1))))

(example
  (<<->> (println :foo :boo :loo))
  > :loo :boo :foo
  :=> nil)

(defmacro <<->
  "Alias for `<->`"
  [form]
  {::aloop {::before-thread? true}}
  `(<-> ~form))

(example
  (<<-> (println :foo :boo))
  > :boo :foo
  :=> nil)

(block "Threading macros, useful with " #'<-> #'<<-> #'<->> #<<->>)

(defmacro |>
  "Same as `->`, but if stumbles across any macro marked as :before-thread? - then firstly executes its form and
  then threads"
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (if `(get-in (meta (var ~(first form))) [::aloop ::before-thread?])
                         (let [[op form] form]
                           `(~op ~(-natural-> x form)))
                         (-natural-> x form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(example
  (|> 0 inc inc str keyword (<-> (println :1 :3 :4 :5)))
  > :1 :2 :3 :4 :5
  :=> nil)

(defmacro <|
  "Same as `->>`, but if stumbles across any macro marked as :before-thread? - then firstly executes its form and
  then threads"
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (if `(get-in (meta (var ~(first form))) [::aloop ::before-thread?])
                         (let [[op form] form]
                           `(~op ~(-natural->> x form)))
                         (-natural->> x form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(example
  (<| 5 dec str keyword (<->> (println :1 :2 :3 :5)))
  > :1 :2 :3 :4 :5
  :=> nil)
