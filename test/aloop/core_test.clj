(ns aloop.core-test
  (:require [clojure.test :refer :all])
  (:require [aloop.core :refer [if- if-> if-|> if->> if-<| if+ if+> if+|> if+>> if+<|
                                with-> with-|> with->> with-<|
                                |-| |-> |->> <-| <<-| <-> <->> <<-> <<->>
                                over-> over->> <-over <<-over
                                |> <|
                                sneak
                                ->map cond-map
                                rotate add->>seq add->seq replace->>seq replace->seq swap
                                mix switch-> switch->> <-switch <<-switch]]
            [clojure.string :as str]))
(def ^:dynamic *invocation-args* [])

(defn ?fn? [& args] args)

(defn ?fn-recording? [& args] (alter-var-root #'*invocation-args* (fn [_] args)))

(deftest if--test
  (is (= (if- (keyword? 'bar)
              :foo)
         :foo))
  (is (= (if- (keyword? :bar)
              :foo)
         :bar)))

(deftest if->-test
  (is (= (if-> (keyword? 'bar)
               name
               clojure.string/upper-case
               keyword)
         :BAR))
  (is (= (if-> (keyword? :bar)
               name
               clojure.string/upper-case
               keyword)
         :bar))
  (is (= (if-> (keyword? 'bar))
         'bar))
  (is (= (if-> (keyword? :bar))
         :bar)))

(deftest if-|>-test
  (is (= (if-|> (keyword? 'bar)
                name
                clojure.string/upper-case
                keyword)
         :BAR))
  (is (= (if-|> (keyword? :bar)
                name
                clojure.string/upper-case
                keyword)
         :bar))
  (is (= (if-|> (keyword? 'bar))
         'bar))
  (is (= (if-|> (keyword? :bar))
         :bar))
  (is (= (if-|> (keyword? 'bar)
                name
                clojure.string/upper-case
                keyword
                (<-> (?fn? :1 :2 :3 :4)))
         [:1 :BAR :2 :3 :4])))

(deftest if->>-test
  (is (= (if->> (keyword? 'bar)
                name
                (clojure.string/replace "abc" #"c")
                keyword)
         :abbar))
  (is (= (if->> (keyword? :bar)
                name
                (clojure.string/replace "abc" #"c")
                keyword)
         :bar))
  (is (= (if->> (keyword? 'bar))
         'bar))
  (is (= (if->> (keyword? :bar))
         :bar)))

(deftest if-<|-test
  (is (= (if-<| (keyword? :bar)
                name
                (clojure.string/replace "abc" #"c")
                keyword)
         :bar))
  (is (= (if-<| (symbol? :bar)
                name
                (clojure.string/replace "abc" #"c")
                keyword)
         :abbar))
  (is (= (if-<| (keyword? 'bar))
         'bar))
  (is (= (if-<| (keyword? :bar))
         :bar))
  (is (= (if-<| (keyword? 'bar)
                name
                clojure.string/upper-case
                keyword
                (<->> (?fn? :1 :2 :3 :4)))
         [:1 :2 :3 :BAR :4])))

(deftest if+-test
  (is (= (if+ (keyword? :bar)
              :foo)
         :foo))
  (is (= (if+ (keyword? 'bar)
              :foo)
         'bar)))

(deftest if+>-test
  (is (= (if+> (keyword? :bar)
               name
               clojure.string/upper-case
               keyword)
         :BAR))
  (is (= (if+> (keyword? 'bar)
               name
               clojure.string/upper-case
               symbol)
         'bar)))

(deftest if+|>-test
  (is (= (if+|> (keyword? :bar)
                name
                clojure.string/upper-case
                keyword)
         :BAR))
  (is (= (if+|> (keyword? 'bar)
                name
                clojure.string/upper-case
                keyword)
         'bar))
  (is (= (if+|> (keyword? 'bar))
         'bar))
  (is (= (if+|> (keyword? :bar))
         :bar))
  (is (= (if+|> (keyword? :bar)
                name
                clojure.string/upper-case
                keyword
                (<-> (?fn? :1 :2 :3 :4)))
         [:1 :BAR :2 :3 :4])))

(deftest if+>>-test
  (is (= (if+>> (keyword? :bar)
                name
                (clojure.string/replace "abc" #"c")
                keyword)
         :abbar))
  (is (= (if+>> (keyword? 'bar)
                name
                (clojure.string/replace "abc" #"c")
                keyword)
         'bar))
  (is (= (if+>> (keyword? 'bar))
         'bar))
  (is (= (if+>> (keyword :bar))
         :bar)))

(deftest if+<|-test
  (is (= (if+<| (keyword? :bar)
                name
                (clojure.string/replace "abc" #"c")
                keyword)
         :abbar))
  (is (= (if+<| (keyword? 'bar)
                name
                (clojure.string/replace "abc" #"c")
                keyword)
         'bar))
  (is (= (if+<| (keyword :bar)
                (<->> (?fn? :1 :2 :3 :4)))
         [:1 :2 :3 :bar :4])))

(deftest with->-test
  (is (= (with-> 0
                 inc
                 ?fn?
                 inc
                 (?fn? 1))
         [0 1])))

(deftest with-|>-test
  (is (= (with-|> 0
                  inc
                  (<-> (?fn? 1 2 3)))
         [1 0 2 3])))

(deftest with->>-test
  (is (= (with->> 0
                  inc
                  ?fn?
                  inc
                  (?fn? 1))
         [1 0])))

(deftest with-<|-test
  (is (= (with-<| 0
                  inc
                  (<->> (?fn? 1 2 3)))
         [1 2 0 3])))

(deftest |-|-test
  (are [x y] (= x y)
             (|-| 0 0 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :4 :5]
             (|-| 0 1 (?fn? :1 :2 :3 :4 :5)) [:2 :1 :3 :4 :5]
             (|-| 0 2 (?fn? :1 :2 :3 :4 :5)) [:3 :2 :1 :4 :5]
             (|-| 0 3 (?fn? :1 :2 :3 :4 :5)) [:4 :2 :3 :1 :5]
             (|-| 0 4 (?fn? :1 :2 :3 :4 :5)) [:5 :2 :3 :4 :1]
             (|-| 0 5 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :4 :5]
             (|-| 0 6 (?fn? :1 :2 :3 :4 :5)) [:2 :1 :3 :4 :5]
             (|-| 0 -1 (?fn? :1 :2 :3 :4 :5)) [:5 :2 :3 :4 :1]
             (|-| 0 -2 (?fn? :1 :2 :3 :4 :5)) [:4 :2 :3 :1 :5]
             (|-| 0 -3 (?fn? :1 :2 :3 :4 :5)) [:3 :2 :1 :4 :5]
             (|-| 0 -4 (?fn? :1 :2 :3 :4 :5)) [:2 :1 :3 :4 :5]
             (|-| 0 -5 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :4 :5]
             (|-| 0 -6 (?fn? :1 :2 :3 :4 :5)) [:5 :2 :3 :4 :1]
             (|-| 0 -7 (?fn? :1 :2 :3 :4 :5)) [:4 :2 :3 :1 :5]
             (|-| -2 -88 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :4 :3 :5]
             (|-| 99 -102 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :5 :4]
             (|-| -3728 78372 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :4 :5]
             (|-| -1000000 8383 (?fn? :1 :2 :3 :4 :5)) [:4 :2 :3 :1 :5]))

(deftest |->-test
  (are [x y] (= x y)
             (|-> 0 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :4 :5]
             (|-> 1 (?fn? :1 :2 :3 :4 :5)) [:2 :1 :3 :4 :5]
             (|-> 2 (?fn? :1 :2 :3 :4 :5)) [:3 :2 :1 :4 :5]
             (|-> 3 (?fn? :1 :2 :3 :4 :5)) [:4 :2 :3 :1 :5]
             (|-> 4 (?fn? :1 :2 :3 :4 :5)) [:5 :2 :3 :4 :1]
             (|-> 5 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :4 :5]
             (|-> 6 (?fn? :1 :2 :3 :4 :5)) [:2 :1 :3 :4 :5]
             (|-> -1 (?fn? :1 :2 :3 :4 :5)) [:5 :2 :3 :4 :1]
             (|-> -2 (?fn? :1 :2 :3 :4 :5)) [:4 :2 :3 :1 :5]
             (|-> -3 (?fn? :1 :2 :3 :4 :5)) [:3 :2 :1 :4 :5]
             (|-> -4 (?fn? :1 :2 :3 :4 :5)) [:2 :1 :3 :4 :5]
             (|-> -5 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :4 :5]
             (|-> -6 (?fn? :1 :2 :3 :4 :5)) [:5 :2 :3 :4 :1]
             (|-> -7 (?fn? :1 :2 :3 :4 :5)) [:4 :2 :3 :1 :5]))

(deftest |->>-test
  (are [x y] (= x y)
             (|->> 0 (?fn? :1 :2 :3 :4 :5)) [:5 :2 :3 :4 :1]
             (|->> 1 (?fn? :1 :2 :3 :4 :5)) [:1 :5 :3 :4 :2]
             (|->> 2 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :5 :4 :3]
             (|->> 3 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :5 :4]
             (|->> 4 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :4 :5]
             (|->> 5 (?fn? :1 :2 :3 :4 :5)) [:5 :2 :3 :4 :1]
             (|->> 6 (?fn? :1 :2 :3 :4 :5)) [:1 :5 :3 :4 :2]
             (|->> -1 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :4 :5]
             (|->> -2 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :5 :4]
             (|->> -3 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :5 :4 :3]
             (|->> -4 (?fn? :1 :2 :3 :4 :5)) [:1 :5 :3 :4 :2]
             (|->> -5 (?fn? :1 :2 :3 :4 :5)) [:5 :2 :3 :4 :1]
             (|->> -6 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :4 :5]
             (|->> -7 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :5 :4]))

(deftest <-|-test
  (are [x y] (= x y)
             (<-| 0 (?fn? :1 :2 :3 :4 :5)) [:5 :2 :3 :4 :1]
             (<-| 1 (?fn? :1 :2 :3 :4 :5)) [:4 :2 :3 :1 :5]
             (<-| 2 (?fn? :1 :2 :3 :4 :5)) [:3 :2 :1 :4 :5]
             (<-| 3 (?fn? :1 :2 :3 :4 :5)) [:2 :1 :3 :4 :5]
             (<-| 4 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :4 :5]
             (<-| 5 (?fn? :1 :2 :3 :4 :5)) [:5 :2 :3 :4 :1]
             (<-| 6 (?fn? :1 :2 :3 :4 :5)) [:4 :2 :3 :1 :5]
             (<-| -1 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :4 :5]
             (<-| -2 (?fn? :1 :2 :3 :4 :5)) [:2 :1 :3 :4 :5]
             (<-| -3 (?fn? :1 :2 :3 :4 :5)) [:3 :2 :1 :4 :5]
             (<-| -4 (?fn? :1 :2 :3 :4 :5)) [:4 :2 :3 :1 :5]
             (<-| -5 (?fn? :1 :2 :3 :4 :5)) [:5 :2 :3 :4 :1]
             (<-| -6 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :4 :5]
             (<-| -7 (?fn? :1 :2 :3 :4 :5)) [:2 :1 :3 :4 :5]))

(deftest <<-|-test
  (are [x y] (= x y)
             (<<-| 0 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :4 :5]
             (<<-| 1 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :5 :4]
             (<<-| 2 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :5 :4 :3]
             (<<-| 3 (?fn? :1 :2 :3 :4 :5)) [:1 :5 :3 :4 :2]
             (<<-| 4 (?fn? :1 :2 :3 :4 :5)) [:5 :2 :3 :4 :1]
             (<<-| 5 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :4 :5]
             (<<-| 6 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :5 :4]
             (<<-| -1 (?fn? :1 :2 :3 :4 :5)) [:5 :2 :3 :4 :1]
             (<<-| -2 (?fn? :1 :2 :3 :4 :5)) [:1 :5 :3 :4 :2]
             (<<-| -3 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :5 :4 :3]
             (<<-| -4 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :5 :4]
             (<<-| -5 (?fn? :1 :2 :3 :4 :5)) [:1 :2 :3 :4 :5]
             (<<-| -6 (?fn? :1 :2 :3 :4 :5)) [:5 :2 :3 :4 :1]
             (<<-| -7 (?fn? :1 :2 :3 :4 :5)) [:1 :5 :3 :4 :2]))

(deftest <->-test
  (is (= (<-> (?fn? :1 :2 :3 :4 :5))
         [:2 :1 :3 :4 :5]))
  (is (= (<-> (?fn? :1))
         [:1]))
  (is (= (<-> (?fn?))
         nil)))

(deftest <->>-test
  (is (= (<->> (?fn? :1 :2 :3 :4 :5))
         [:1 :2 :3 :5 :4]))
  (is (= (<->> (?fn? :1))
         [:1]))
  (is (= (<->> (?fn?))
         nil)))

(deftest <<->>-test
  (is (= (<<->> (?fn? :1 :2 :3 :4 :5))
         [:5 :2 :3 :4 :1]))
  (is (= (<<->> (?fn? :1))
         [:1]))
  (is (= (<<->> (?fn?))
         nil)))

(deftest <<->-test
  (is (= (<<-> (?fn? :1 :2 :3 :4 :5))
         [:2 :1 :3 :4 :5]))
  (is (= (<<-> (?fn? :1))
         [:1]))
  (is (= (<<-> (?fn?))
         nil)))

(deftest |>-test
  (is (= (|> 0
             inc
             inc
             str
             keyword
             (<-> (?fn? :1 :3 :4 :5)))
         [:1 :2 :3 :4 :5])))

(deftest <|-test
  (is (= (<| 5
             dec
             str
             keyword
             (<->> (?fn? :1 :2 :3 :5)))
         [:1 :2 :3 :4 :5])))

(deftest rotate-test
  (are [x y] (= x y)
             (rotate 3 0) 0
             (rotate 3 1) 1
             (rotate 3 2) 2
             (rotate 3 3) 0
             (rotate 3 4) 1
             (rotate 3 -1) 2
             (rotate 3 -2) 1
             (rotate 3 -3) 0
             (rotate 3 -4) 2))

(deftest add->>seq-test
  (are [x y] (= x y)
             ((add->>seq 5) '(1 2 3 4)) '(1 2 3 4 5)
             (add->>seq '(1 2 3 4) 5) '(1 2 3 4 5)
             (add->>seq '(1 2 3 4) 5 0) '(1 2 3 4 5)
             (add->>seq '(1 2 3 4) 5 1) '(1 2 3 5 4)
             (add->>seq '(1 2 3 4) 5 -1) '(5 1 2 3 4)))

(deftest add->seq-test
  (are [x y] (= x y)
             (add->seq [1 2 3 4] 5) [5 1 2 3 4]
             ((add->seq 5) '(1 2 3 4)) '(5 1 2 3 4)
             (add->seq '(1 2 3 4) 5) '(5 1 2 3 4)
             (add->seq '(1 2 3 4) 5 3) '(1 2 3 5 4)
             (add->seq '(1 2 3 4) 5 0) '(5 1 2 3 4)
             (add->seq '(1 2 3 4) 5 -1) '(1 2 3 4 5)))

(deftest replace->>seq-test
  (are [x y] (= x y)
             ((replace->>seq 4) '(1 2 3)) '(1 2 4)
             (replace->>seq '(1 2 3) 4) '(1 2 4)
             (replace->>seq '(1 2 3) 4 0) '(1 2 4)
             (replace->>seq '(1 2 3) 4 1) '(1 4 3)
             (replace->>seq '(1 2 3) 4 -1) '(4 2 3)))

(deftest replace->seq-test
  (are [x y] (= x y)
             ((replace->seq 4) '(1 2 3)) '(4 2 3)
             (replace->seq '(1 2 3) 4) '(4 2 3)
             (replace->seq '(1 2 3) 4 0) '(4 2 3)
             (replace->seq '(1 2 3) 4 -1) '(1 2 4)))

(deftest swap-test
  (are [x y] (= x y)
             ((swap 0 3) '(1 2 3 4 5)) '(4 2 3 1 5)
             (swap '(1 2 3 4 5) 0 0) '(1 2 3 4 5)
             (swap '(1 2 3 4 5) 0 3) '(4 2 3 1 5)
             (swap '(1 2 3 4 5) 0 -1) '(5 2 3 4 1)
             (swap '(1 2 3 4 5) -1 -2) '(1 2 3 5 4)))

(deftest mix-test
  (are [x y] (= x y)
             ((mix 0 1 ?fn? 1 2 3) 4 5)
             [2 1 3 4 5]))

(deftest sneak-test
  (let [s (with-out-str (sneak (-> 0 inc inc inc)))]
    (is (str/includes? s "file\t: [aloop/core_test.clj:"))
    (is (str/includes? s "form\t: (-> 0 inc inc inc)"))
    (is (str/includes? s "result\t: 3"))))

(deftest switch->-test
  (is (= (switch-> :arg1 ?fn? :arg2 :arg3)
         (?fn? :arg1 :arg2 :arg3))))

(deftest swtch->>test
  (is (= (switch->> :arg3 ?fn? :arg1 :arg2)
         (?fn? :arg1 :arg2 :arg3))))

(deftest <-swtch-test
  (is (= (<-switch ?fn? :arg1 :arg2 :arg3)
         (?fn? :arg3 :arg2 :arg1))))

(deftest <<-swtch-test
  (is (= (<<-switch ?fn? :arg1 :arg2 :arg3)
         (?fn? :arg1 :arg2 :arg3))))

(deftest over->-test
  (is (= (over-> :arg1 (?fn-recording? :arg2 :arg3))
         :arg1))
  (is (= *invocation-args*
         [:arg1 :arg2 :arg3]))
  (is (= (-> 1 inc (over-> (-> inc inc (?fn-recording? 2 1))) inc)
         3))
  (is (= *invocation-args*
         [4 2 1])))

(deftest over->>-test
  (is (= (over->> :arg1 (?fn-recording? :arg2 :arg3))
         :arg1))
  (is (= *invocation-args*
         [:arg2 :arg3 :arg1])))

(deftest <-over-test
  (is (= (<-over (?fn-recording? :arg2 :arg3) :arg1)
         :arg1))
  (is (= *invocation-args*
         [:arg1 :arg2 :arg3]))
  (is (= (->> 1 inc (<-over (-> inc inc (?fn-recording? 2 1))) inc)
         3))
  (is (= *invocation-args*
         [4 2 1])))

(deftest <<-over-test
  (is (= (<<-over (?fn-recording? :arg2 :arg3) :arg1)
         :arg1))
  (is (= *invocation-args*
         [:arg2 :arg3 :arg1])))

(deftest ->map-test
  (let [a 'a
        d 'd]
    (is (= (->map a {:b 'b :c 'c} d)
           {:a a :b 'b :c 'c :d 'd}))))
