(ns arrows.test.proc
  (:require [clojure.test :refer :all]
            [arrows.proc :refer :all]
            [monads.core :as m]))

(deftest test-get-state
  (is (= [:state :state]
         ((m/bind get-state parser) :state))))

(deftest test-update-state
  (is (= [:new-state :new-state]
         ((update-state (constantly :new-state)) :state))))

(deftest test-get-val
  (is (= [1 {:a 1}]
         ((get-val :a) {:a 1})))
  (is (= [2 {:a 1}]
         ((m/bind (get-val :a)
                  (comp parser inc)) {:a 1}))))

(deftest test-set-val
  (is [5 {:a 5}]
      ((set-val :a 5) {})))

(deftest test-next-item
  (is (= [:a {:cmd [:b :c]}]
         ((m/bind next-item parser) {:cmd [:a :b :c]}))))

(deftest test-item-test
  (is (= [:a {:cmd [:b :c]}]
         ((m/bind (item-test (partial = :a)) parser) {:cmd [:a :b :c]})))
  (is (= nil
         ((m/bind (item-test (partial = :b)) parser) {:cmd [:a :b :c]}))))

(deftest test-is-item
  (is (= [:a {:cmd [:b :c]}]
         ((m/bind (is-item :a) parser) {:cmd [:a :b :c]})))
  (is (= nil
         ((m/bind (is-item :b) parser) {:cmd [:a :b :c]}))))

(deftest test-parser-do
  (is (= [:a :state]
         ((parser-do [x (parser :a)] x) :state)))
  (is (= [:a {:cmd [:b :c]}]
         ((parser-do [x next-item] x) {:cmd [:a :b :c]})))
  (is (= [1 {:a 1 :cmd [:b :c]}]
         ((parser-do [x next-item
                 y (get-val :a)]
                y) {:a 1 :cmd [:a :b :c]}))))

(defn replace-autos [expr]
  (cond
   (and (symbol? expr)
        (.contains (name expr) "auto")) (-> (name expr)
                                            (.split  "_")
                                            first
                                            symbol)
   (seq? expr) (map replace-autos expr)
   (vector? expr) (vec (map replace-autos expr))
   :else expr))

(deftest test-match-binding-clause
  (is (= nil
         (match-binding-clause {:arr 'arr :env '[x] :cmd '(x <- arrow <- )})))
  (is (= '[(arrows/seq (arr (clojure.core/partial clojure.core/repeat 2))
                      (arrows/par (arr clojure.core/identity)
                                  (arrows/seq (arr (clojure.core/fn [[x]]
                                                                    (inc x)))
                                              arrow)))
          (arr (clojure.core/partial clojure.core/apply clojure.core/conj))]
         (-> {:arr 'arr :env '[x] :cmd '(x <- arrow -< (inc x))}
             match-binding-clause
             first
             replace-autos)))
  (is (= '[(arrows/seq (arr (clojure.core/partial clojure.core/repeat 2))
                       (arrows/par (arr clojure.core/identity)
                                   arrow))
           (arr (clojure.core/partial clojure.core/apply clojure.core/conj))]
         (-> {:arr 'arr :env '[x] :cmd '(x <- arrow -< _)}
             match-binding-clause
             first
             replace-autos))))

(deftest test-match-let-clause
  (is (= nil
         (match-let-clause {:arr 'arr :env '[x] :cmd '(let [a :x b :y])})))
  (is (= '[(arrows/all (arr clojure.core/identity)
                       (arr (clojure.core/fn [[x]]
                                             (clojure.core/let [a :x
                                                                b :y]
                                                               [a b]))))
           (arr (clojure.core/partial clojure.core/apply clojure.core/concat))]
         (-> {:arr 'arr :env '[x] :cmd '(:let [a :x b :y])}
             match-let-clause
             first
             replace-autos))))

(deftest test-match-let-do
  (is (thrown-with-msg? Exception
        #"Malformed 'proc-do'"
        (match-do {:arr 'arr :env '[x] :cmd '(proc-do [let [a :x b :y]] :bogus)})))
  (is (= '(arrows/seq (arr clojure.core/vector)
                     (arrows/all (arr clojure.core/identity)
                                 (arr (clojure.core/fn [[x]]
                                                       (clojure.core/let [a :x
                                                                          b :y]
                                                                         [a b]))))
                     (arr (clojure.core/partial clojure.core/apply clojure.core/concat))
                     (arrows/seq (arr (clojure.core/partial clojure.core/repeat 2))
                                 (arrows/par (arr clojure.core/identity)
                                             (arrows/seq (arr (clojure.core/fn [[x a b]] x))
                                                         arrow)))
                     (arr (clojure.core/partial clojure.core/apply clojure.core/conj))
                     (arrows/seq (arr (clojure.core/partial clojure.core/repeat 2))
                                 (arrows/par (arr clojure.core/identity)
                                             another))
                     (arr (clojure.core/partial clojure.core/apply clojure.core/conj))
                     (arr (clojure.core/fn [[y z x a b]]
                                           (assoc {} a 10 b 5))))
         (-> {:arr 'arr :env '[x] :cmd '(proc-do [:let [a :x b :y]
                                                  z <- arrow -< x
                                                  y <- another -< _]
                                                 (assoc {} a 10 b 5))}
             match-do
             first
             replace-autos))))

(deftest test-match-application
  (is (= '(arrows/seq (arr (clojure.core/fn [v] (clojure.core/let [x v] (inc x)))) arrow)
         (-> {:arr 'arr :env '[x] :cmd '(arrow (inc x))}
             match-application
             first
             replace-autos)))
  (is (= nil
         (match-application {:arr 'arr :env '[x] :cmd '(arrow )}))))

(deftest test-match-if
  (is (= '(arrows/seq (arr (clojure.core/fn [v]
                                            (clojure.core/let [x v]
                                              [(clojure.core/boolean (= x 10)) v])))
                      (arrows/cond
                       true true-arrow
                       (quote arrows.proc/_) (arrows/seq a1 a2)))
         (-> {:arr 'arr :env '[x] :cmd '(proc-if (= x 10)
                                                 true-arrow
                                                 (arrows/seq a1 a2))}
             match-if
             first
             replace-autos)))
  (is (= nil
         (match-if {:arr 'arr :env '[x] :cmd '(if arrow)}))))

(deftest test-match-proc
  (is (thrown-with-msg? Exception
        #"Malformed 'proc-do'*"
        (match-proc {:arr 'arr :env '[x] :cmd '(proc-do [:let [a :x b :y]
                                                         z <- arrow -< a
                                                         y <- another -< ]
                                                        (assoc {} a 10 b 5))})))
  (is (= '(arrows/seq (arr clojure.core/vector)
                     (arrows/all (arr clojure.core/identity)
                                 (arr (clojure.core/fn [[x]]
                                                       (clojure.core/let [a :x
                                                                          b :y]
                                                                         [a b]))))
                     (arr (clojure.core/partial clojure.core/apply clojure.core/concat))
                     (arrows/seq (arr (clojure.core/partial clojure.core/repeat 2))
                                 (arrows/par (arr clojure.core/identity)
                                             (arrows/seq (arr (clojure.core/fn [[x a b]] x))
                                                         arrow)))
                     (arr (clojure.core/partial clojure.core/apply clojure.core/conj))
                     (arrows/seq (arr (clojure.core/partial clojure.core/repeat 2))
                                 (arrows/par (arr clojure.core/identity)
                                             another))
                     (arr (clojure.core/partial clojure.core/apply clojure.core/conj))
                     (arr (clojure.core/fn [[y z x a b]] (assoc {} a 10 b 5))))
         (-> {:arr 'arr :env '[x] :cmd '(proc-do
                                         [:let [a :x b :y]
                                          z <- arrow -< x
                                          y <- another -< _]
                                         (assoc {} a 10 b 5))}
             match-proc
             first
             replace-autos)))
  (is (= '(arrows/seq (arr (clojure.core/fn [v]
                                            (clojure.core/let [x v]
                                              [(clojure.core/boolean (= x 10)) v])))
                      (arrows/cond
                       true true-arrow
                       (quote arrows.proc/_) (arrows/seq a1 a2)))
         (-> {:arr 'arr :env '[x] :cmd '(proc-if (= x 10)
                                                 true-arrow
                                                 (arrows/seq a1 a2))}
             match-proc
             first
             replace-autos)))
  (is (= '(arrows/seq (arr (clojure.core/fn [v] (clojure.core/let (x v) (inc x))))
                      arrow)
         (-> {:arr 'arr :env '[x] :cmd '(arrow (inc x))}
             match-proc
             first
             replace-autos))))
