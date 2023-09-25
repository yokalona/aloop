(ns aloop.core-test
  (:require [clojure.test :refer :all])
  (:require [aloop.core :refer [if- if-> if->> -if <-if <<-if
                                if+ if+> if+>> +if <+if <<+if]]
            [clojure.string :as str]))

(def -default {:all-invocations []
               :last-invocation []})

(defn true-fn [] true)
(defn false-fn [] false)

(def fn-invocation-spy
  (atom -default))

(defn exec&return-first
  [ret _]
  ret)

(defn exec&return-last
  [_ ret]
  ret)

(defn ?fn?
  ([arg]
   (swap! fn-invocation-spy
          (fn [old new]
            {:all-invocations (conj (:all-invocations old) new)
             :last-invocation new})
          arg)
   arg)
  ([arg & args]
   (swap! fn-invocation-spy
          (fn [old new]
            {:all-invocations (conj (:all-invocations old) new)
             :last-invocation new})
          (cons arg args))
   (cons arg args)))

(defn ?clear?
  []
  (reset! fn-invocation-spy -default))

(defn clear-fixture
  [f]
  (?clear?)
  (f))

(use-fixtures :each clear-fixture)

(deftest if--test
  (testing "If- behaves as if in terms of args"
    (is (nil? (if-)))
    (is (nil? (if- (keyword? 'i-am-not-a-keyword))))
    (is (nil? (if- nil)))
    (is (nil? (if- 1))))
  (testing "First argument of predicate is returned if predicate is true, otherwise - else is executed in do block"
    (testing "Do block"
      (is (= :else (if- (keyword? 'i-am-not-a-keyword) :else)))
      (is (true? (if- (keyword? 'i-am-not-a-keyword) :else (true-fn)))))
    (is (= :arg (if- (keyword? :arg) (?fn? :fail))))
    (is (empty? (:last-invocation @fn-invocation-spy)))
    (is (= :else (if- (exec&return-first false (?fn? :ret)) :else)))
    (is (= :ret (:last-invocation @fn-invocation-spy)))
    (testing "Predicate arg executed only once"
      (?clear?)
      (is (= true (if- (exec&return-first (?fn? true) 'ignore) :else)))
      (is (= true (:last-invocation @fn-invocation-spy)))))
  (testing "Empty predicate and no arg predicate"
    (is (= :else (if- (false-fn) :else)))
    (is (nil? (if- (true-fn) :else)))
    (is (= :else (if- false :else)))
    (is (nil? (if- true :else)))))

(deftest if->-test
  (testing "If-> behaves as if in terms of args"
    (is (nil? (if->)))
    (is (nil? (if-> (keyword? 'i-am-not-a-keyword))))
    (is (nil? (if-> nil)))
    (is (nil? (if-> 1))))
  (testing "First argument of predicate is returned if predicate is true, otherwise - 'threads first' else on this"
    (testing "Threading"
      (is (= :i-am-not-a-keyword (if-> (keyword? 'i-am-not-a-keyword) keyword)))
      (is (= :i-am-a-keyword (if-> (keyword? 'i-am-not-a-keyword) name (str/replace #"-not" "") keyword))))
    (is (= :arg (if-> (keyword? :arg) (?fn? :fail))))
    (is (empty? (:last-invocation @fn-invocation-spy)))
    (is (true? (if-> (exec&return-first false (?fn? :ret)) false?)))
    (is (= :ret (:last-invocation @fn-invocation-spy)))
    (testing "Predicate arg executed only once"
      (?clear?)
      (is (= true (if-> (exec&return-first (?fn? true) 'ignore) :else)))
      (is (= true (:last-invocation @fn-invocation-spy)))))
  (testing "Empty predicate and no arg predicate"
    (is (true? (if-> (false-fn) nil?)))
    (is (nil? (if-> (true-fn) nil?)))
    (is (true? (if-> false nil?)))
    (is (nil? (if-> true nil?)))))

(deftest if->>-test
  (testing "If->> behaves as if in terms of args"
    (is (nil? (if->>)))
    (is (nil? (if->> (keyword? 'i-am-not-a-keyword))))
    (is (nil? (if->> nil)))
    (is (nil? (if->> 1))))
  (testing "First argument of predicate is returned if predicate is true, otherwise - 'threads last' else on this"
    (testing "Threading"
      (is (= :i-am-not-a-keyword (if->> (keyword? 'i-am-not-a-keyword) keyword)))
      (is (= :i-am-truly-keyword (if->> (keyword? 'truly) name (str/replace "i-am-not-a-keyword" #"not-a") keyword))))
    (is (= :arg (if->> (keyword? :arg) (?fn? :fail))))
    (is (empty? (:last-invocation @fn-invocation-spy)))
    (is (true? (if->> (exec&return-first false (?fn? :ret)) false?)))
    (is (= :ret (:last-invocation @fn-invocation-spy)))
    (testing "Predicate arg executed only once"
      (?clear?)
      (is (= true (if->> (exec&return-first (?fn? true) 'ignore) :else)))
      (is (= true (:last-invocation @fn-invocation-spy)))))
  (testing "Empty predicate and no arg predicate"
    (is (true? (if->> (false-fn) nil?)))
    (is (nil? (if->> (true-fn) nil?)))
    (is (true? (if->> false nil?)))
    (is (nil? (if->> true nil?)))))

(deftest -if-test
  (testing "-If behaves as if in terms of args"
    (is (nil? (-if)))
    (is (nil? (-if (keyword? 'i-am-not-a-keyword))))
    (is (nil? (-if nil)))
    (is (nil? (-if 1))))
  (testing "Last argument of predicate is returned if predicate is true, otherwise - else is executed in do block"
    (testing "Do block"
      (is (= :else (-if (keyword? 'i-am-not-a-keyword) :else)))
      (is (true? (-if (keyword? 'i-am-not-a-keyword) :else (true-fn)))))
    (testing "Last arg"
      (is (= "c" (-if (str/ends-with? "abc" "c") :else))))
    (is (= :arg (-if (keyword? :arg) (?fn? :fail))))
    (is (empty? (:last-invocation @fn-invocation-spy)))
    (is (= :else (-if (exec&return-last (?fn? :ret) false) :else)))
    (is (= :ret (:last-invocation @fn-invocation-spy)))
    (testing "Predicate arg executed only once"
      (?clear?)
      (is (= true (-if (exec&return-last 'ignore (?fn? true)) :else)))
      (is (= true (:last-invocation @fn-invocation-spy)))))
  (testing "Empty predicate and no arg predicate"
    (is (= :else (-if (false-fn) :else)))
    (is (nil? (-if (true-fn) :else)))
    (is (= :else (-if false :else)))
    (is (nil? (-if true :else)))))

(deftest <-if-test
  (testing "<-if behaves as if in terms of args"
    (is (nil? (<-if)))
    (is (nil? (<-if (keyword? 'i-am-not-a-keyword))))
    (is (nil? (<-if nil)))
    (is (nil? (<-if 1))))
  (testing "Last argument of predicate is returned if predicate is true, otherwise - 'threads first' else on this"
    (testing "Threading"
      (is (= :i-am-not-a-keyword (<-if (keyword? 'i-am-not-a-keyword) keyword)))
      (is (= :i-am-a-keyword (<-if (keyword? 'i-am-not-a-keyword) name (str/replace #"-not" "") keyword))))
    (testing "Last arg"
      (is (= "c" (<-if (str/ends-with? "abc" "c") :else))))
    (is (= :arg (<-if (keyword? :arg) (?fn? :fail))))
    (is (empty? (:last-invocation @fn-invocation-spy)))
    (is (true? (<-if (exec&return-last (?fn? :ret) false) false?)))
    (is (= :ret (:last-invocation @fn-invocation-spy)))
    (testing "Predicate arg executed only once"
      (?clear?)
      (is (= true (<-if (exec&return-last 'ignore (?fn? true)) :else)))
      (is (= true (:last-invocation @fn-invocation-spy)))))
  (testing "Empty predicate and no arg predicate"
    (is (true? (<-if (false-fn) nil?)))
    (is (nil? (<-if (true-fn) nil?)))
    (is (true? (<-if false nil?)))
    (is (nil? (<-if true nil?)))))

(deftest <<-if-test
  (testing "<<-if behaves as if in terms of args"
    (is (nil? (<<-if)))
    (is (nil? (<<-if (keyword? 'i-am-not-a-keyword))))
    (is (nil? (<<-if nil)))
    (is (nil? (<<-if 1))))
  (testing "Last argument of predicate is returned if predicate is true, otherwise - 'threads first' else on this"
    (testing "Threading"
      (is (= :i-am-not-a-keyword (<<-if (keyword? 'i-am-not-a-keyword) keyword)))
      (is (= :i-am-truly-keyword (<<-if (keyword? 'truly) name (str/replace "i-am-not-a-keyword" #"not-a") keyword))))
    (testing "Last arg"
      (is (= "c" (<-if (str/ends-with? "abc" "c") :else))))
    (is (= :arg (<<-if (keyword? :arg) (?fn? :fail))))
    (is (empty? (:last-invocation @fn-invocation-spy)))
    (is (true? (<<-if (exec&return-last (?fn? :ret) false) false?)))
    (is (= :ret (:last-invocation @fn-invocation-spy)))
    (testing "Predicate arg executed only once"
      (?clear?)
      (is (= true (<<-if (exec&return-last 'ignore (?fn? true)) :else)))
      (is (= true (:last-invocation @fn-invocation-spy)))))
  (testing "Empty predicate and no arg predicate"
    (is (true? (<<-if (false-fn) nil?)))
    (is (nil? (<<-if (true-fn) nil?)))
    (is (true? (<<-if false nil?)))
    (is (nil? (<<-if true nil?)))))

(deftest if+-test
  (testing "If+ behaves as if in terms of args"
    (is (nil? (if+)))
    (is (nil? (if+ (keyword? 'i-am-not-a-keyword))))
    (is (nil? (if+ nil)))
    (is (nil? (if+ 1))))
  (testing "First argument of predicate is returned if predicate is true, otherwise - then is executed in do block"
    (testing "Do block"
      (is (= :else (if+ (keyword? :i-am-not-a-keyword) :else)))
      (is (true? (if+ (keyword? :i-am-not-a-keyword) :else (true-fn)))))
    (is (= 'arg (if+ (keyword? 'arg) (?fn? :fail))))
    (is (empty? (:last-invocation @fn-invocation-spy)))
    (is (= :else (if+ (exec&return-first true (?fn? :ret)) :else)))
    (is (= :ret (:last-invocation @fn-invocation-spy)))
    (testing "Predicate arg executed only once"
      (?clear?)
      (is (false? (if+ (exec&return-first (?fn? false) 'ignore) :else)))
      (is (false? (:last-invocation @fn-invocation-spy)))))
  (testing "Empty predicate and no arg predicate"
    (is (nil? (if+ (false-fn) :else)))
    (is (= :else (if+ (true-fn) :else)))
    (is (nil? (if+ false :else)))
    (is (= :else (if+ true :else)))))

(deftest if+>-test
  (testing "If+> behaves as if in terms of args"
    (is (nil? (if+>)))
    (is (nil? (if+> (keyword? 'i-am-not-a-keyword))))
    (is (nil? (if+> nil)))
    (is (nil? (if+> 1))))
  (testing "First argument of predicate is returned if predicate is true, otherwise - then is executed in do block"
    (testing "Threading"
      (is (= "i-am-not-a-keyword" (if+> (keyword? :i-am-not-a-keyword) name)))
      (is (= :i-am-a-keyword (if+> (keyword? :i-am-not-a-keyword) name (str/replace #"-not" "") keyword))))
    (is (= 'arg (if+> (keyword? 'arg) (?fn? :fail))))
    (is (empty? (:last-invocation @fn-invocation-spy)))
    (is (true? (if+> (exec&return-first true (?fn? :ret)) true?)))
    (is (= :ret (:last-invocation @fn-invocation-spy)))
    (testing "Predicate arg executed only once"
      (?clear?)
      (is (false? (if+> (exec&return-first (?fn? false) 'ignore) :else)))
      (is (false? (:last-invocation @fn-invocation-spy)))))
  (testing "Empty predicate and no arg predicate"
    (is (nil? (if+> (false-fn) :else)))
    (is (true? (if+> (true-fn) nil?)))
    (is (nil? (if+> false :else)))
    (is (true? (if+> true nil?)))))

(deftest if+>>-test
  (testing "If+>> behaves as if in terms of args"
    (is (nil? (if+>>)))
    (is (nil? (if+>> (keyword? 'i-am-not-a-keyword))))
    (is (nil? (if+>> nil)))
    (is (nil? (if+>> 1))))
  (testing "First argument of predicate is returned if predicate is true, otherwise - then is executed in do block"
    (testing "Threading"
      (is (= "i-am-not-a-keyword" (if+>> (keyword? :i-am-not-a-keyword) name)))
      (is (= :i-am-truly-keyword (if+>> (keyword? :truly) name (str/replace "i-am-not-a-keyword" #"not-a") keyword))))
    (is (= 'arg (if+>> (keyword? 'arg) (?fn? :fail))))
    (is (empty? (:last-invocation @fn-invocation-spy)))
    (is (true? (if+>> (exec&return-first true (?fn? :ret)) true?)))
    (is (= :ret (:last-invocation @fn-invocation-spy)))
    (testing "Predicate arg executed only once"
      (?clear?)
      (is (false? (if+>> (exec&return-first (?fn? false) 'ignore) :else)))
      (is (false? (:last-invocation @fn-invocation-spy)))))
  (testing "Empty predicate and no arg predicate"
    (is (nil? (if+>> (false-fn) :else)))
    (is (true? (if+>> (true-fn) nil?)))
    (is (nil? (if+>> false :else)))
    (is (true? (if+>> true nil?)))))

(deftest +if-test
  (testing "+If behaves as if in terms of args"
    (is (nil? (+if)))
    (is (nil? (+if (keyword? 'i-am-not-a-keyword))))
    (is (nil? (+if nil)))
    (is (nil? (+if 1))))
  (testing "First argument of predicate is returned if predicate is true, otherwise - then is executed in do block"
    (testing "Do block"
      (is (= :else (+if (keyword? :i-am-not-a-keyword) :else)))
      (is (true? (+if (keyword? :i-am-not-a-keyword) :else (true-fn)))))
    (is (= 'arg (+if (keyword? 'arg) (?fn? :fail))))
    (is (empty? (:last-invocation @fn-invocation-spy)))
    (is (= :else (+if (exec&return-first true (?fn? :ret)) :else)))
    (is (= :ret (:last-invocation @fn-invocation-spy)))
    (testing "Predicate arg executed only once"
      (?clear?)
      (is (false? (+if (exec&return-first 'ignore (?fn? false)) :else)))
      (is (false? (:last-invocation @fn-invocation-spy)))))
  (testing "Empty predicate and no arg predicate"
    (is (nil? (+if (false-fn) :else)))
    (is (= :else (+if (true-fn) :else)))
    (is (nil? (+if false :else)))
    (is (= :else (+if true :else)))))

(deftest <+if-test
  (testing "<+If behaves as if in terms of args"
    (is (nil? (<+if)))
    (is (nil? (<+if (keyword? 'i-am-not-a-keyword))))
    (is (nil? (<+if nil)))
    (is (nil? (<+if 1))))
  (testing "First argument of predicate is returned if predicate is true, otherwise - then is executed in do block"
    (testing "Threading"
      (is (= "i-am-not-a-keyword" (<+if (keyword? :i-am-not-a-keyword) name)))
      (is (= :i-am-a-keyword (<+if (keyword? :i-am-not-a-keyword) name (str/replace #"-not" "") keyword))))
    (is (= 'arg (<+if (keyword? 'arg) (?fn? :fail))))
    (is (empty? (:last-invocation @fn-invocation-spy)))
    (is (true? (<+if (exec&return-first (?fn? :ret) true) true?)))
    (is (= :ret (:last-invocation @fn-invocation-spy)))
    (testing "Predicate arg executed only once"
      (?clear?)
      (is (false? (<+if (exec&return-first 'ignore (?fn? false)) :else)))
      (is (false? (:last-invocation @fn-invocation-spy)))))
  (testing "Empty predicate and no arg predicate"
    (is (nil? (<+if (false-fn) :else)))
    (is (true? (<+if (true-fn) nil?)))
    (is (nil? (<+if false :else)))
    (is (true? (<+if true nil?)))))

(deftest <<+if-test
  (testing "<<+If behaves as if in terms of args"
    (is (nil? (<<+if)))
    (is (nil? (<<+if (keyword? 'i-am-not-a-keyword))))
    (is (nil? (<<+if nil)))
    (is (nil? (<<+if 1))))
  (testing "First argument of predicate is returned if predicate is true, otherwise - then is executed in do block"
    (testing "Threading"
      (is (= "i-am-not-a-keyword" (<<+if (keyword? :i-am-not-a-keyword) name)))
      (is (= :i-am-truly-keyword (<<+if (keyword? :truly) name (str/replace "i-am-not-a-keyword" #"not-a") keyword))))
    (is (= 'arg (<<+if (keyword? 'arg) (?fn? :fail))))
    (is (empty? (:last-invocation @fn-invocation-spy)))
    (is (true? (<<+if (exec&return-first (?fn? :ret) true) true?)))
    (is (= :ret (:last-invocation @fn-invocation-spy)))
    (testing "Predicate arg executed only once"
      (?clear?)
      (is (false? (<<+if (exec&return-first 'ignore (?fn? false)) :else)))
      (is (false? (:last-invocation @fn-invocation-spy)))))
  (testing "Empty predicate and no arg predicate"
    (is (nil? (<<+if (false-fn) :else)))
    (is (true? (<<+if (true-fn) nil?)))
    (is (nil? (<<+if false :else)))
    (is (true? (<<+if true nil?)))))

(deftest -|
  )