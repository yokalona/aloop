(ns aloop.core
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

(declare
  ;; Divers set of IFs, usually for singular branch.
  if- if-> if-|> if->> if-<| if+ if+> if+|> if+>> if+<|
  ;; Declarative operations for over throwing args
  with-> with-|> with->> with-<|
  ;; Function argument order manipulation
  |-| |-> |->> <-| <<-| <-> <->> <<-> <<->>
  ;; Threading
  |> <|
  ;; Debug examination
  sneak
  ;; Usability
  ->map cond-map
  ;; Sequential modification
  rotate add->>seq add->seq replace->>seq replace->seq swap
  ;; Functions arguments manipulation
  mix over switch-> switch->> <-switch <<-switch
  ;; private
  -pretty-str -before-thread?)

(defn- -natural->
  [x form]
  (with-meta `(~(first form) ~x ~@(next form)) (meta form)))

(defn- -natural->>
  [x form]
  (with-meta `(~(first form) ~@(next form) ~x) (meta form)))

(defmacro if-
  "Takes `predicate p` of at least one argument and a list of `forms`.
  If predicate is true - returns its first argument, otherwise
  executes `else` forms in the implicit `do`

  **Examples**

  ```clojure
  (if- (keyword? 'bar)
    :foo)

  :=> :foo

  (if- (keyword? :bar)
    :foo)

  :=> :bar
  ```"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p arg (cons 'do else))))

(defmacro if->
  "Takes `predicate p` of at least one argument and a list of `forms`.
  If predicate is true - returns its first argument, otherwise
  threads `else` forms with `ps first arg`

  **Examples**

  ```clojure
  (if-> (keyword? 'bar)
        name
        clojure.string/upper-case
        keyword)

  :=> :BAR

  (if-> (keyword? :bar)
        name
        clojure.string/upper-case
        keyword)

  :=> :bar

  ```"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p arg `(-> ~arg ~@else))))

(defmacro if-|>
  "Same as `if->`, but uses `|>` instead of `->`

  **Examples**

  ```clojure
  (defn ?fn? [& args] args)

  (if-|> (keyword? 'bar)
         name
         clojure.string/upper-case
         keyword
         (<-> (?fn? :1 :2 :3 :4)))
  :=> [:1 :BAR :2 :3 :4]
  ```

  See [[|>]]"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p arg `(|> ~arg ~@else))))

(defmacro if->>
  "Takes `predicate p` of at least one argument and a list of `forms`.
  If predicate is true - returns its first argument, otherwise
  threads last `else` forms with `ps first arg`

  **Examples**

  ```clojure
  (if->> (keyword? 'bar)
         name
         (clojure.string/replace \"abc\" #\"c\")
         keyword)

  :=> :abbar

  (if->> (keyword? :bar)
         name
         (clojure.string/replace \"abc\" #\"c\")
         keyword)

  :=> :bar
  ```"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p arg `(->> ~arg ~@else))))

(defmacro if-<|
  "Same as `if->>`, but uses `<|` instead of `->>`

  **Examples**

  ```clojure
  (if-<| (keyword? 'bar)
         name
         clojure.string/upper-case
         keyword
         (<->> (?fn? :1 :2 :3 :4)))

  :=> [:1 :2 :3 :BAR :4]
  ```"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p arg `(<| ~arg ~@else))))

(defmacro if+
  "Takes `predicate p` of at least one argument and a list of `forms`.
  If predicate is false - returns its first argument, otherwise
  executes `then` forms in the implicit `do`

  **Examples**

  ```clojure
  (if+ (keyword? :bar)
    :foo)

  :=> :bar

  (if+ (keyword? 'bar)
    :foo)

  :=> :foo
  ```"
  [p & then]
  (if-let [arg (second p)]
    (list 'if p (if (= 1 (count then))
                  (first then)
                  (cons 'do then)) arg)))

(defmacro if+>
  "Takes `predicate p` of at least one argument and a list of `forms`.
  If predicate is false - returns its first argument, otherwise
  threads `then` forms with `ps first arg`

  **Examples**

  ```clojure
  (if+> (keyword? :bar)
        name
        clojure.string/upper-case
        keyword)

  :=> :BAR

  (if+> (keyword? 'bar)
        name
        clojure.string/upper-case
        keyword)

  :=> 'bar
  ```"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p `(-> ~arg ~@else) arg)))

(defmacro if+|>
  "Same as `if+>`, but uses `|>` instead of `->`

  **Examples**

  ```clojure
  (if+|> (keyword? :bar)
         name
         clojure.string/upper-case
         keyword
         (<-> (?fn? :1 :2 :3 :4)))

  :=> [:1 :BAR :2 :3 :4]
  ```"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p `(|> ~arg ~@else) arg)))

(defmacro if+>>
  "Takes `predicate p` of at least one argument and a list of `forms`.
  If predicate is false - returns its first argument, otherwise
  threads last `then` forms with `ps first arg`

  **Examples**

  ```clojure
  (if+>> (keyword? :bar)
         name
         (clojure.string/replace \"abc\" #\"c\")
         keyword)

  :=> :abbar

  (if+>> (keyword? 'bar)
         name
         (clojure.string/replace \"abc\" #\"c\")
         keyword)
  :=> 'bar
  ```"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p `(->> ~arg ~@else) arg)))

(defmacro if+<|
  "Same as `if+>>`, but uses `<|` instead of `->>`

  **Examples**

  ```clojure
  (if+<| (keyword? :bar)
         name
         (clojure.string/replace \"abc\" #\"c\")
         keyword)

  :=> :abbar

  (if+<| (keyword? 'bar)
         name
         (clojure.string/replace \"abc\" #\"c\")
         keyword)

  :=> 'bar
  ```"
  [p & else]
  (if-let [arg (second p)]
    (list 'if p `(<| ~arg ~@else) arg)))

(defmacro with->
  "Takes `x` and a list of `forms` and executes `forms` in implicit `do` providing
  `x` as first argument to every `form` in `forms`. Unlike `->` doesn't thread and
  executes in a procedure manner

  **Examples**

  ```clojure
  (with-> 0
          inc
          ?fn?
          inc
          ?fn?)

  :=> [0]
  ```"
  [x & forms]
  (loop [d (list 'do), forms forms]
    (if forms
      (let [form (first forms)
            with (if (seq? form)
                   (-natural-> x form)
                   (list form x))]
        (recur (concat d (list with)) (next forms)))
      d)))

(defmacro with-|>
  "Same as `with->`, but uses `|>` instead of `->`

  **Examples**

  ```clojure
  (with-|> 0
           inc
           (<-> (?fn? 1 2 3)))

  :=> [1 0 2 3]
  ```"
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

(defmacro with->>
  "Takes `x` and a list of `forms` and executes `forms` in implicit `do` providing
  `x` as last argument to every `form` in `forms`. Unlike `->>` doesn't thread and
  executes in a procedure manner.

  **Examples**

  ```clojure
  (with->> 0
           inc
           ?fn?
           inc
           (?fn? 1))

  :=> [1 0]
  ```"
  [x & forms]
  (loop [d (list 'do), forms forms]
    (if forms
      (let [form (first forms)
            with (if (seq? form)
                   (-natural->> x form)
                   (list form x))]
        (recur (concat d (list with)) (next forms)))
      d)))

(defmacro with-<|
  "Same as `with->>`, but uses `<|` instead of `->>`

  **Examples**

  ```clojure
  (with-<| 0
           inc
           (<->> (?fn? 1 2 3)))

  :=> [1 2 0 3]
  ```"
  [x & forms]
  (loop [d (list 'do), forms forms]
    (if forms
      (let [form (first forms)
            with (if (seq? form)
                   (if `(get-in (meta (var ~(first form))) [::aloop ::before-thread?])
                     (let [[op form] form]
                       `(~op ~(-natural->> x form)))
                     (-natural->> x form)))]
        (recur (concat d (list with)) (next forms)))
      d)))

(defmacro |-|
  "Takes a `form` and two indices: `l` and `r`. Switch places `lth` arg with `rth` arg of `form` if any. Positively
  rotates both indices, i.e. counts indices forward, where first element is 0, second is 1 and so on. If index is
  negative - counts indices negatively, i.e. last element is -1, second to last - -2.

  **Examples**

  ```clojure
  (|-| 0 0 (?fn? :1 :2 :3 :4 :5))

  :=> [:1 :2 :3 :4 :5]

  (|-| 0 -1 (?fn? :1 :2 :3 :4 :5))

  :=> [:5 :2 :3 :4 :1]

  (|-| -2 -88 (?fn? :1 :2 :3 :4 :5))

  :=> [:1 :2 :4 :3 :5]

  (|-| -1000000 8383 (?fn? :1 :2 :3 :4 :5))

  :=> [:4 :2 :3 :1 :5]
  ```

  See: [[rotate]]"
  [l r form]
  {::aloop {::before-thread? true}}
  (if+ (coll? form)
       (cons (first form) (swap (next form) l r))))

(defmacro |->
  "Takes a `form` and index `i`. Switch places for `1st` arg with `ith` arg of `form` if any. Positively rotates index
  `i`, i.e. counts indices forward, where first element is 0, second is 1 and so on. If index is negative - counts
  indices negatively, i.e. last element is -1, second to last - -2.

  **Examples**

  ```clojure
  (|-> 0 (?fn? :1 :2 :3 :4 :5))

  :=> [:1 :2 :3 :4 :5]

  (|-> 1 (?fn? :1 :2 :3 :4 :5))

  :=> [:2 :1 :3 :4 :5]

  (|-> -1 (?fn? :1 :2 :3 :4 :5))

  :=> [:5 :2 :3 :4 :1]

  (|-> -102 (?fn? :1 :2 :3 :4 :5))

  :=> [:4 :2 :3 :1 :5]
  ```

  See: [[rotate]]"
  {::aloop {::before-thread? true}}
  [i form]
  (if+ (coll? form)
       (cons (first form) (swap (next form) i 0))))

(defmacro |->>
  "Takes a `form` and index `i`. Switch places for `last` arg with `ith` arg of `form` if any. Positively rotates index
  `i`, i.e. counts indices forward, where first element is 0, second is 1 and so on. If index is negative - counts
  indices negatively, i.e. last element is -1, second to last - -2.

  **Examples**

  ```clojure
  (|->> 0 (?fn? :1 :2 :3 :4 :5))

  :=> [:5 :2 :3 :4 :1]

  (|->> 1 (?fn? :1 :2 :3 :4 :5))

  :=> [:1 :5 :3 :4 :2]

  (|->> -1 (?fn? :1 :2 :3 :4 :5))

  :=> [:1 :2 :3 :4 :5]

  (|->> -4 (?fn? :1 :2 :3 :4 :5))

  :=> [:1 :5 :3 :4 :2]
  ```

  See: [[rotate]]"
  {::aloop {::before-thread? true}}
  [i form]
  (if+ (coll? form)
       (cons (first form) (swap (next form) i -1))))

(defmacro <-|
  "Takes a `form` and index `i`. Switch places for `1st` arg with `ith` arg of `form` if any. Negatively rotates index
  `i`, i.e. counts indices backwards, where last element is 0, second to last is 1 and so on. If index is negative -
  counts indices positively, i.e. first element is -1, second - -2.

  **Examples**

  ```clojure
  (<-| 0 (?fn? :1 :2 :3 :4 :5))

  :=> [:5 :2 :3 :4 :1]

  (<-| 1 (?fn? :1 :2 :3 :4 :5))

  :=> [:4 :2 :3 :1 :5]

  (<-| -1 (?fn? :1 :2 :3 :4 :5))

  :=> [:1 :2 :3 :4 :5]

  (<-| -4 (?fn? :1 :2 :3 :4 :5))

  :=> [:4 :2 :3 :1 :5]
  ```

  See: [[rotate]]"
  {::aloop {::before-thread? true}}
  [i form]
  `(|-> ~(- (inc i)) ~form))

(defmacro <<-|
  "Takes a `form` and index `i`. Switch places for `last` arg with `ith` arg of `form` if any. Negatively rotates index
  `i`, i.e. counts indices backwards, where last element is 0, second to last is 1 and so on. If index is negative -
  counts indices positively, i.e. first element is -1, second - -2.

  **Examples**

  ```clojure
  (<<-| 0 (?fn? :1 :2 :3 :4 :5))

  :=> [:1 :2 :3 :4 :5]

  (<<-| 1 (?fn? :1 :2 :3 :4 :5))

  :=> [:1 :2 :3 :5 :4]

  (<<-| -1 (?fn? :1 :2 :3 :4 :5))

  :=> [:5 :2 :3 :4 :1]

  (<<-| -4 (?fn? :1 :2 :3 :4 :5))

  :=> [:1 :2 :3 :5 :4]
  ```

  See: [[rotate]]"
  {::aloop {::before-thread? true}}
  [i form]
  `(|->> ~(- (inc i)) ~form))

(defmacro <->
  "Takes a form of any args and switch places for first and second arg if any

  **Examples**

  ```clojure
  (<-> (?fn? :1 :2 :3 :4 :5))

  :=> [:2 :1 :3 :4 :5]

  (<-> (?fn? :1))

  :=> [:1]

  (<-> (?fn?))

  :=> nil
  ```"
  {::aloop {::before-thread? true}}
  [form]
  `(|-| 0 1 ~form))

(defmacro <->>
  "Take a form of any args and switch places for last and second to last arg if any

  **Examples**

  ```clojure
  (<->> (?fn? :1 :2 :3 :4 :5))

  :=> [:1 :2 :3 :5 :4]

  (<->> (?fn? :1))

  :=> [:1]

  (<->> (?fn?))

  :=> nil
  ```"
  {::aloop {::before-thread? true}}
  [form]
  `(|-| -2 -1 ~form))

(defmacro <<->>
  "Take a form of any args and switch places for first and last arg if any

  **Examples**

  ```clojure
  (<<->> (?fn? :1 :2 :3 :4 :5))

  :=> [:5 :2 :3 :4 :1]

  (<<->> (?fn? :1))

  :=> [:1]

  (<<->> (?fn?))

  :=> nil
  ```"
  {::aloop {::before-thread? true}}
  [form]
  `(|-| 0 -1 ~form))

(defmacro <<->
  "Alias for `<->`

  **Examples**

  ```clojure
  (<<-> (?fn? :1 :2 :3 :4 :5))

  :=> [:2 :1 :3 :4 :5]

  (<<-> (?fn? :1))

  :=> [:1]

  (<<-> (?fn?))

  :=> nil
  ```"
  {::aloop {::before-thread? true}}
  [form]
  `(<-> ~form))

(defmacro |>
  "Same as `->`, but if stumbles across any macro marked as :before-thread? - then firstly executes its form and
  then threads

  **Examples**

  ```clojure
  (|> 0
      inc
      inc
      str
      keyword
      (<-> (?fn? :1 :3 :4 :5)))

  :=> [:1 :2 :3 :4 :5]
  ```"
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (if (-> form first resolve meta ::aloop ::before-thread? some?)
                         (let [[op form] form]
                           `(~op ~(-natural-> x form)))
                         (-natural-> x form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(defmacro <|
  "Same as `->>`, but if stumbles across any macro marked as :before-thread? - then firstly executes its form and
  then threads

  **Examples**

  ```clojure
  (<| 5
      dec
      str
      keyword
      (<->> (println :1 :2 :3 :5)))

  :=> [:1 :2 :3 :4 :5]
  ```"
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (if (-> form first resolve meta ::aloop ::before-thread? some?)
                         (let [[op form] form]
                           `(~op ~(-natural->> x form)))
                         (-natural->> x form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(defmacro sneak
  "Prints to *out* the result of `form` and `form` itself. Doesn't execute form until later"
  ^{::aloop {::before-thread? true}}
  [form]
  (let [f (-pretty-str form 3)
        file *file*
        line `~(:line (meta &form))
        f (if+<| (str/includes? f "\n") (str "\n"))]
    `(let [result# ~form]
       (println (format "sneak:\n\tfile\t: [%s:%s]\n\tform\t: %s\n\tresult\t: %s" ~file ~line ~f result#))
       result#)))

(defmacro ->map
  "Creates map from provided `vars`

  **Examples**

  ```clojure
  (let [a 1
        b 2
        c 3]
        (->map a b c {:d 4}))

  :=> {:a 1 :b 2 :c 3 :d 4}
  ```"
  [& keys]
  (reduce (fn [acc x]
            (if (map? x)
              (merge acc x)
              (assoc acc (keyword x) x))) {} keys))

(defmacro cond-map
  [col]
  (reduce (fn [acc x] (-> acc (add->>seq (key x)) (add->>seq (val x))))
          (list 'cond)
          col))

(defn rotate
  "Takes an `index`and right border `c`. Envelops `index` around the length of `c`, if `index` is positive - envelopes
  clockwise, otherwise - counterclockwise. In other words, uses different approaches for indexing a collection of
  length c. For positive values of `index` - count a first element of collection as 0, second as 1 and so on.
  For negative values - last element of collection will be -1, second to last - -2. In other words, provides cyclic
  indexation for an array of length `c`.

  **Examples**

  | array 'arr'       |  1  |  2  |  3  |  4  |  5  |
  |-------------------|-----|-----|-----|-----|-----|
  | positive indexing |  0  |  1  |  2  |  3  |  4  |
  | positive indexing | -5  | -4  | -3  | -2  | -1  |
  | negative indexing | -4  | -3  | -2  | -1  |  0  |
  | negative indexing |  1  |  2  |  3  |  4  |  5  |

  ```clojure
  (rotate (count arr) 0) :=> 0
  (rotate (count arr) 3) :=> 3
  (rotate (count arr) 5) :=> 0
  (rotate (count arr) 6) :=> 1
  (rotate (count arr) -1) :=> 4
  ```"
  [c index]
  (if (neg? index)
    (recur c (+ c index))
    (if+ (>= index c)
         (recur c (mod index c)))))

(defn add->>seq
  "Takes an arbitrary collection `where` and an element `what`. Adds `what` to the index `place` in `where`.
  If no `place` provided - adds `what` to the end of `where`. If only `what` provided - returns transducer.
  Negatively rotates index `place`, i.e. counts indices backwards, where last element is 0, second to last
  is 1 and so on.

  **Examples**

  ```clojure
  ((add->>seq 5) '(1 2 3 4))

  :=> '(1 2 3 4 5)

  (add->>seq '(1 2 3 4) 5)

  :=> '(1 2 3 4 5)

  (add->>seq '(1 2 3 4) 5 0)

  :=> '(1 2 3 4 5)

  (add->>seq '(1 2 3 4) 5 1)

  :=> '(1 2 3 5 4)
  (add->>seq '(1 2 3 4) 5 -1)

  :=> '(5 1 2 3 4)
  ```"
  ([what]
   (fn [where]
     (add->>seq where what)))
  ([where what]
   (if+ (sequential? where)
        (->> where
             reverse
             (cons what)
             reverse)))
  ([where what place]
   (add->seq where what (- (count where) place))))

(defn add->seq
  "Takes an arbitrary collection `where` and an element `what`. Adds `what` to the index `place` in `where`.
  If no `place` provided - adds `what` to the beginning of `where`. If only `what` provided - returns transducer.
  Positively rotates index `place`, i.e. counts indices forward, where first element is 0, second is 1 and so on.

  **Examples**

  ```clojure
  ((add->seq 5) '(1 2 3 4))

  :=> '(5 1 2 3 4)

  (add->seq '(1 2 3 4) 5)

  :=> '(5 1 2 3 4)

  (add->seq '(1 2 3 4) 5 3)

  :=> '(1 2 3 5 4)

  (add->seq '(1 2 3 4) 5 0)

  :=> '(5 1 2 3 4)

  (add->seq '(1 2 3 4) 5 -1)

  :=> '(1 2 3 4 5)
  ```"
  ([what]
   (fn [where]
     (add->seq where what)))
  ([where what]
   (add->seq where what 0))
  ([where what place]
   (if+ (sequential? where)
        (let [place (-> where count inc (rotate place))]
          (->> where
               (drop place)
               (concat (add->>seq (take place where) what)))))))

(defn replace->>seq
  "Takes an arbitrary collection `where` and an element `what`. Replaces `what` on the index `place` in `where`.
  If no `place` provided - changes nothing. If only `what` provided - returns transducer. Negatively rotates index
  `place`, i.e. counts indices backwards, where last element is 0, second to last is 1 and so on.

  **Examples**

  ```clojure
  ((replace->>seq 4) '(1 2 3))

  :=> '(1 2 4)

  (replace->>seq '(1 2 3) 4)

  :=> '(1 2 4)

  (replace->>seq '(1 2 3) 4 0)

  :=> '(1 2 4)

  (replace->>seq '(1 2 3) 4 1)

  :=> '(1 4 3)

  (replace->>seq '(1 2 3) 4 -1)

  :=> '(4 2 3)
  ```"
  ([what]
   (fn [where]
     (replace->>seq where what)))
  ([where what]
   (if+ (sequential? where)
        (if- (empty? where)
             (-> where
                 (drop-last)
                 (add->>seq what)))))
  ([where what place]
   (replace->seq where what (-> where count (- place 1)))))

(defn replace->seq
  "Takes an arbitrary collection `where` and an element `what`. Replaces `what` on the index `place` in `where`.
  If no `place` provided - changes nothing. If only `what` provided - returns transducer. Positively rotates index
  `place`, i.e. counts indices forward, where first element is 0, second is 1 and so on.

  **Examples**

  ```clojure
  ((replace->seq 4) '(1 2 3))

  :=> '(1 2 4)

  (replace->seq '(1 2 3) 4)

  :=> '(1 2 4)

  (replace->seq '(1 2 3) 4 0)

  :=> '(4 2 3)

  (replace->seq '(1 2 3) 4 -1)

  :=> '(1 2 4)
  ```"
  ([what]
   (fn [where]
     (replace->seq where what)))
  ([where what]
   (replace->seq where what 0))
  ([where what place]
   (if+ (sequential? where)
        (let [place (-> where count (rotate place))]
          (-> place
              (take where)
              (concat (drop (inc place) where))
              (add->seq what place))))))

(defn swap
  "Takes an arbitrary collection `where` and two indices: `left` and `right`. Swaps value on the `left` with the value
  on the `right`. Positively rotates both indices `place`, i.e. counts indices forward, where first element is 0, second
  is 1 and so on.

  **Examples**

  ```clojure
  ((swap 0 3) '(1 2 3 4 5))

  :=> '(4 2 3 1 5)

  (swap '(1 2 3 4 5) 0 0)

  :=> '(1 2 3 4 5)

  (swap '(1 2 3 4 5) 0 3)

  :=> '(4 2 3 1 5)

  (swap '(1 2 3 4 5) 0 -1)

  :=> '(5 2 3 4 1)

  (swap '(1 2 3 4 5) -1 -2)

  :=> '(1 2 3 5 4)
  ```"
  ([left right]
   (fn [where]
     (swap where left right)))
  ([where left right]
   (if+ (sequential? where)
        (if- (empty? where)
             (let [left-value (nth where (rotate (count where) left))
                   right-value (nth where (rotate (count where) right))]
               (-> where
                   (replace->seq right-value left)
                   (replace->seq left-value right)))))))

(defn mix
  "Takes a function `f` with fewer than normal arguments and two indices: `l` and `r`. Switch places `lth` arg with
  `rth` arg of `form` if any. Returns new function with args switched. Positively rotates both indices, i.e. counts
  indices forward, where first element is 0, second is 1 and so on. If index is negative - counts indices negatively,
  i.e. last element is -1, second to last - -2.

  **Examples**

  ```clojure
  (mix 0 1 (?fn? :1 :2 :3) :4 :5)

  :=> [:2 :1 :3 :4 :5]
  ```

  See: [[rotate]] [[|-|]]"
  [l r & args]
  (fn [& fargs]
    (|> args
        next
        (concat fargs)
        (swap l r)
        (<-> (apply (first args))))))

(defn switch->
  "Takes one `argument` and a function `f` with fewer than normal amount of args. Passes first argument to `f` in first
  position. Acts exactly as
  ```clojure
  (switch-> :arg1 ?fn? :arg2 :arg3) ==> (f :arg1 :arg2 :arg3)
  ```"
  [arg f & args]
  (apply f (add->seq args arg)))

(defn switch->>
  "Takes one `argument` and a function `f` with fewer than normal amount of args. Passes first argument to `f` in last
  position. Acts exactly as
  ```clojure
  (switch->> :arg1 ?fn? :arg2 :arg3) ==> (f :arg2 :arg3 :arg1)
  ```"
  [arg f & args]
  (apply f (add->>seq args arg)))

(defn <-switch
  "Takes one `argument` and a function `f` with fewer than normal amount of args. Passes `argument` to `f` in first
  position. Acts exactly as
  ```clojure
  (<-switch ?fn? :arg1 :arg2 :arg3) ==> (f :arg3 :arg1 :arg2)
  ```"
  [f & args]
  (apply f (swap args 0 -1)))

(defn <<-switch
  "Takes one `argument` and a function `f` with fewer than normal amount of args. Passes `argument` to `f` in last
  position. Acts exactly as
  ```clojure
  (<<-switch ?fn? :arg1 :arg2 :arg3) ==> (f :arg1 :arg2 :arg3)
  ```"
  [f & args]
  (apply f args))

(defn over
  "Overthrows arguments applying f to args"
  [f & args]
  (apply f args)
  args)

(defn- -pretty-str
  [val indentation]
  (<| val pprint/pprint
      (pprint/with-pprint-dispatch pprint/code-dispatch)
      with-out-str
      str/trim-newline
      (<<-> (str/replace #"\n" (str "\n" (apply str (repeat indentation "\t")))))))
