(ns aloop.core
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [aloop.test :refer [multiple?]]))

(declare
  ;; One branch if's
  if- if-> if->> -if <-if <<-if
  if+ if+> if+>> +if <+if <<+if
  ;; Sequential execution
  with-| with-|| with-> with->> |-with ||-with <-with <<-with

  ;; Sequential modification
  +rotate -rotate add->>seq add->seq replace->>seq replace->seq swap
  ;; PRIVATE
  -universal-if- -natural-> -natural->> -push-over-> -seq-over-> -form-over->)

(defn +rotate
  [c index]
  (if (neg? index)
    (recur c (+ c index))
    (if (>= index c)
      (recur c (mod index c))
      index)))

(defn -rotate
  [c index]
  (+rotate c (- (inc index))))

(defn- -pretty-str
  [val indentation]
  (let [a (->> val
               pprint/pprint
               (pprint/with-pprint-dispatch pprint/code-dispatch)
               with-out-str
               str/trim-newline)]
    (str/replace a #"\n" (str "\n" (apply str (repeat indentation "\t"))))))

(defn- -if-
  [index predicate else f up]
  (if (multiple? predicate)
    (let [index (-> predicate next count (+rotate index))
          invocation (first predicate)
          predicate (next predicate)]
      `(let [arg# ~(nth predicate index)]
         (if (~up (~invocation ~@(take index predicate) arg# ~@(drop (inc index) predicate)))
           (~f arg# ~@else)
           arg#)))
    `(if (~up ~predicate)
       (~f nil ~@else))))

(defmacro if-
  "Takes a `predicate` and a body `else`. If predicate is FALSE executes `else` in explicit do block, otherwise returns
  predicate's first argument. In case if predicate doesn't have arguments, no `else` or no predicate provided - returns
  `nil`.

  **Examples**

  ```clojure
  (if- (keyword? 'i-am-not-a-keyword)
       :else)

  :=> :else

  (if- (keyword? :arg)
       :else)

  :=> :arg
  ```

  **Note: Side effects in predicate might cause unwonted behavior**
  See [[do]]"
  ([] nil)
  ([_predicate] nil)
  ([predicate & else]
   (-if- 0 predicate else 'do false?)))

(defmacro if->
  "Takes a `predicate` and a body `else`. If predicate is FALSE threads `else` with the first argument of a predicate,
  otherwise returns predicate's first argument. In case if predicate doesn't have arguments - threads on nil. If no
  `else` or no predicate provided - returns `nil`.

  **Examples**

  ```clojure
  (if-> (keyword? 'i-am-not-a-keyword)
        name
        (str/replace #\"-not\" \"\")
        keyword)

  :=> :i-am-a-keyword

  (if-> (keyword? :arg)
        ?fn?)

  :=> :arg
  ```

  **Note: Side effects in predicate might cause unwonted behavior**
  See [[->]]"
  ([] nil)
  ([_predicate] nil)
  ([predicate & else]
   (-if- 0 predicate else '-> false?)))

(defmacro if->>
  "Takes a `predicate` and a body `else`. If predicate is FALSE threads last `else` with the first argument of a
  predicate, otherwise returns predicate's first argument. In case if predicate doesn't have arguments - threads on nil.
  If no `else` or no predicate provided - returns `nil`.

  **Examples**

  ```clojure
  (if->> (keyword? 'truly)
         name
         (str/replace \"i-am-not-a-keyword\" #\"not-a\")
         keyword)

  :=> :i-am-truly-keyword

  (if->> (keyword? :arg)
         ?fn?)

  :=> :arg
  ```

  **Note: Side effects in predicate might cause unwonted behavior**
  See [[->]]"
  ([] nil)
  ([_predicate] nil)
  ([predicate & else]
   (-if- 0 predicate else '->> false?)))

(defmacro -if
  "Takes a `predicate` and a body `else`. If predicate is FALSE executes `else` in explicit do block, otherwise returns
  predicate's last argument. In case if predicate doesn't have arguments, no `else` or no predicate provided - returns
  `nil`.

  **Examples**

  ```clojure
  (if- (keyword? 'i-am-not-a-keyword)
       :else)

  :=> :else

  (if- (str/ends-with \"\" :arg)
       :else)

  :=> :arg
  ```

  **Note: Side effects in predicate might cause unwonted behavior**
  See [[do]]"
  ([] nil)
  ([_predicate] nil)
  ([predicate & else]
   (-if- -1 predicate else 'do false?)))

(defmacro <-if
  "Takes a `predicate` and a body `else`. If predicate is FALSE threads `else` with last argument of a predicate,
  otherwise returns predicate's last argument. In case if predicate doesn't have arguments - threads on nil.

  **Examples**

  ```clojure
  (<-if (keyword? 'i-am-not-a-keyword)
        str
        str/upper-case))

  :=> \"I-AM-NOT-A-KEYWORD\"

  (<-if (keyword? :arg)
        ?fn?)

  :=> :arg
  ```

  **Note: Side effects in predicate might cause unwonted behavior**
  See [[->]]"
  ([] nil)
  ([_predicate] nil)
  ([predicate & else]
   (-if- -1 predicate else '-> false?)))

(defmacro <<-if
  "Takes a `predicate` and a body `else`. If predicate is FALSE threads last `else` with last argument of a predicate,
  otherwise returns predicate's last argument. In case if predicate doesn't have arguments - threads on nil.

  **Examples**

  ```clojure
  (<<-if (keyword? 'i-am-not-a-keyword)
         str
         str/upper-case))

  :=> \"I-AM-NOT-A-KEYWORD\"

  (<<-if (keyword? :arg)
        ?fn?)

  :=> :arg
  ```

  **Note: Side effects in predicate might cause unwonted behavior**
  See [[->>]]"
  ([] nil)
  ([_predicate] nil)
  ([predicate & else]
   (-if- -1 predicate else '->> false?)))

(defmacro if+
  "Takes a `predicate` and a body `then`. If predicate is TRUE executes `then` in explicit do block, otherwise returns
  predicate's first argument. In case if predicate doesn't have arguments, no `then` or no predicate provided - returns
  `nil`.

  **Examples**

  ```clojure
  (if- (keyword? 'i-am-not-a-keyword)
       :else)

  :=> :else

  (if- (keyword? :arg)
       :else)

  :=> :arg
  ```

  **Note: Side effects in predicate might cause unwonted behavior**
  See [[do]]"
  ([] nil)
  ([_predicate] nil)
  ([predicate & then]
   (-if- 0 predicate then 'do true?)))

(defmacro if+>
  "Takes a `predicate` and a body `then`. If predicate is TRUE threads `then` with the first argument of a predicate,
  otherwise returns predicate's first argument. In case if predicate doesn't have arguments - threads on nil. If no
  `then` or no predicate provided - returns `nil`.

  **Examples**

  ```clojure
  (if-> (keyword? 'i-am-not-a-keyword)
        name
        (str/replace #\"-not\" \"\")
        keyword)

  :=> :i-am-a-keyword

  (if-> (keyword? :arg)
        ?fn?)

  :=> :arg
  ```

  **Note: Side effects in predicate might cause unwonted behavior**
  See [[->]]"
  ([] nil)
  ([_predicate] nil)
  ([predicate & then]
   (-if- 0 predicate then '-> true?)))

(defmacro if+>>
  "Takes a `predicate` and a body `then`. If predicate is TRUE threads last `then` with the first argument of a
  predicate, otherwise returns predicate's first argument. In case if predicate doesn't have arguments - threads on nil.
  If no `then` or no predicate provided - returns `nil`.

  **Examples**

  ```clojure
  (if->> (keyword? 'truly)
         name
         (str/replace \"i-am-not-a-keyword\" #\"not-a\")
         keyword)

  :=> :i-am-truly-keyword

  (if->> (keyword? :arg)
         ?fn?)

  :=> :arg
  ```

  **Note: Side effects in predicate might cause unwonted behavior**
  See [[->]]"
  ([] nil)
  ([_predicate] nil)
  ([predicate & then]
   (-if- 0 predicate then '->> true?)))

(defmacro +if
  "Takes a `predicate` and a body `then`. If predicate is TRUE executes `then` in explicit do block, otherwise returns
  predicate's last argument. In case if predicate doesn't have arguments, no `then` or no predicate provided - returns
  `nil`.

  **Examples**

  ```clojure
  (if- (keyword? 'i-am-not-a-keyword)
       :else)

  :=> :else

  (if- (str/ends-with \"\" :arg)
       :else)

  :=> :arg
  ```

  **Note: Side effects in predicate might cause unwonted behavior**
  See [[do]]"
  ([] nil)
  ([_predicate] nil)
  ([predicate & then]
   (-if- -1 predicate then 'do true?)))

(defmacro <+if
  "Takes a `predicate` and a body `then`. If predicate is TRUE threads `then` with last argument of a predicate,
  otherwise returns predicate's last argument. In case if predicate doesn't have arguments - threads on nil.

  **Examples**

  ```clojure
  (<-if (keyword? 'i-am-not-a-keyword)
        str
        str/upper-case))

  :=> \"I-AM-NOT-A-KEYWORD\"

  (<-if (keyword? :arg)
        ?fn?)

  :=> :arg
  ```

  **Note: Side effects in predicate might cause unwonted behavior**
  See [[->]]"
  ([] nil)
  ([_predicate] nil)
  ([predicate & then]
   (-if- -1 predicate then '-> true?)))

(defmacro <<+if
  "Takes a `predicate` and a body `then`. If predicate is TRUE threads last `then` with last argument of a predicate,
  otherwise returns predicate's last argument. In case if predicate doesn't have arguments - threads on nil.

  **Examples**

  ```clojure
  (<<-if (keyword? 'i-am-not-a-keyword)
         str
         str/upper-case))

  :=> \"I-AM-NOT-A-KEYWORD\"

  (<<-if (keyword? :arg)
        ?fn?)

  :=> :arg
  ```

  **Note: Side effects in predicate might cause unwonted behavior**
  See [[->>]]"
  ([] nil)
  ([_predicate] nil)
  ([predicate & then]
   (-if- -1 predicate then '->> true?)))

(defmacro -|
  "\"Pushes\" `arg` to the `index` position in form.

  **Example**

  ```clojure
  (-| 2 2 (/ 8))

  :=> 4
  ```"
  [arg index form]
  (if form
    (if (coll? form)
      (-push-over-> arg (+rotate (count form) index) form)
      `(~form ~arg))))

(defmacro -||
  [index form arg]
  `(-| ~arg ~index ~form))

(defmacro |-
  [arg index form]
  `(-| ~arg ~(-> form count (-rotate index)) ~form))

(defmacro ||-
  [index form arg]
  `(-| ~arg ~(-> form count (-rotate index)) ~form))

(defmacro |-|
  [l r form]
  (if form
    (if (coll? form)
      (cons (first form)
            (swap (next form)
                  (-> form count (+rotate l))
                  (-> form count (+rotate r)))))))

(defmacro |-||
  [l r form]
  `(|-| ~l ~(-> form count (-rotate r)) ~form))

(defmacro ||-|
  [l r form]
  `(|-| ~(-> form count (-rotate l)) ~r ~form))

(defmacro ||-||
  [l r form]
  `(|-| ~(-> form count (-rotate l)) ~(-> form count (-rotate r)) ~form))

(defmacro |->
  [index form]
  `(|-| 0 ~(-> form count (+rotate index)) ~form))

(defmacro |->>
  [index form]
  `(|-| -1 ~(-> form count (+rotate index)) ~form))

(defmacro ||->
  [index form]
  `(|-| 0 ~(-> form count (-rotate index)) ~form))

(defmacro ||->>
  [index form]
  `(|-| -1 ~(-> form count (-rotate index)) ~form))

(defmacro <-|
  [form index]
  `(|-> ~index ~form))

(defmacro <<-|
  [form index]
  `(|->> ~index ~form))

(defmacro <-||
  [form index]
  `(||-> ~index ~form))

(defmacro <<-||
  [form index]
  `(||->> ~index ~form))

(defmacro <->>
  [form]
  `(|-| 0 -1 ~form))

(defmacro <<->
  [form]
  `(<->> ~form))

(defmacro with-|
  [arg index & forms]
  (let [index (+rotate (count forms) index)]
    (loop [d (list 'do), forms forms]
      (if forms
        (let [form (first forms)
              with (if (seq? form)
                     (-push-over-> arg index form)
                     (list form arg))]
          (recur (concat d (list with)) (next forms)))
        d))))

(defmacro with-||
  [form & rst]
  (let [index (nth (+rotate (count rst) -1) rst)
        arg (last rst)]
    (if (and index (number? index))
      (with-| arg index (cons form (drop-last 2 rst))))))

(defmacro |-with
  [arg index & forms]
  (let [index (-rotate (count forms) index)]
    (loop [d (list 'do), forms forms]
      (if forms
        (let [form (first forms)
              with (if (seq? form)
                     (-push-over-> arg index form)
                     (list form arg))]
          (recur (concat d (list with)) (next forms)))
        d))))

(defmacro ||-with
    [form & rst]
    (let [index (nth (-rotate (count rst) -1) rst)
          arg (last rst)]
      (if (and index (number? index))
        (with-| arg index (cons form (drop-last 2 rst))))))

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
  (if (< 1 (count forms))
    (loop [d (list 'do), forms forms]
    (if forms
      (let [form (first forms)
            with (if (seq? form)
                   (-natural-> x form)
                   (list form x))]
        (recur (concat d (list with)) (next forms)))
      d))
    `(~(first forms) ~x)))

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

;(defmacro <-with
;  [form & rst]
;  (let [arg (last rst)]
;    (with-> arg (cons form (drop-last rst)))))

;(defmacro <<-with
;  [form & rst]
;  (let [arg (last rst)]
;    (with->> arg (cons form (drop-last rst)))))

(defmacro over-|
  [arg index form]
  (if form
    (if (seq? form)
      (-seq-over-> arg (-> form count (+rotate index)) form)
      (-form-over-> arg form))))

(defmacro over-||
  [index form arg]
  (if form
    (if (seq? form)
      (-seq-over-> arg (-> form count (+rotate index)) form)
      (-form-over-> arg form))))

(defmacro |-over
  [arg index form]
  (if form
    (if (seq? form)
      (-seq-over-> arg (-> form count (-rotate index)) form)
      (-form-over-> arg form))))

(defmacro ||-over
  [index form arg]
  (if form
    (if (seq? form)
      (-seq-over-> arg (-> form count (-rotate index)) form)
      (-form-over-> arg form))))

(defmacro over->
  [arg form]
  `(over-| ~arg 0 ~form))

(defmacro over->>
  [form arg]
  `(over-| ~arg 0 ~form))

(defmacro <-over
  [form arg]
  `(|-over ~arg 0 ~form))

(defmacro <<-over
  [form arg]
  `(|-over ~arg 0 ~form))

(defmacro sneak
  "Prints to *out* the result of `form` and `form` itself. Doesn't execute form until later"
  ^{::aloop {::before-thread? true}}
  [form]
  (let [f (-pretty-str form 3)
        file *file*
        line `~(:line (meta &form))
        f (if+>> (str/includes? f "\n") (str "\n"))]
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
  "Same as cond, but takes a map as input

  **Examples**

  ```clojure
  (cond-map {(test? a)        (action a)
             (another-test a) (action b)
             :else            (action c)})

  ;; is equal to

  (cond (test? a) (action a)
        (another-test a) (action b)
        :else (action c))
  ```"
  [col]
  (reduce (fn [acc x] (-> acc (add->>seq (key x)) (add->>seq (val x))))
          (list 'cond)
          col))

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
        (let [place (-> where count inc (+rotate place))]
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
        (let [place (-> where count (+rotate place))]
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
             (let [left-value (nth where (+rotate (count where) left))
                   right-value (nth where (+rotate (count where) right))]
               (-> where
                   (replace->seq right-value left)
                   (replace->seq left-value right)))))))

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


(defn- -seq-over->
  [arg index form]
  (let [index (+rotate (count form) index)]
    `(let [p# ~arg]
       (~(first form) ~@(take index (next form)) p# ~@(drop index (next form)))
       p#)))

(defn- -push-over->
  [arg index form]
  (with-meta `(~(first form) ~@(take index (next form)) ~arg ~@(drop index (next form))) (meta form)))

(defn- -natural->
  [x form]
  (-push-over-> x 0 form))

(defn- -natural->>
  [x form]
  (-push-over-> x -1 form))

(defn- -form-over->
  [arg form]
  `(let [p# ~arg]
     (~form p#)
     p#))