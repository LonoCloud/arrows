(ns arrows.proc
  (:require [monads.core :as m]
            [monads.macros :as mm]))

;; A simple parser to parse proc expressions

(deftype parser-m [v mv f]
  clojure.lang.IFn
  (invoke [_ s]
    (if f
      (if-let [[v ss] (mv s)]
        ((f v) ss)
        nil)
      [v s]))

  m/Monad
  (do-result [_ v]
    (parser-m. v nil nil))
  (bind [mv f]
    (parser-m. nil mv f))

  m/MonadZero
  (zero [_]
    (constantly nil))
  (plus-step [mv mvs]
    (parser-m. nil
               (fn [s]
                 (loop [[mv & mvs] (cons mv mvs)]
                   (when mv
                     (if-let [result (mv s)]
                       result
                       (recur mvs)))))
               (fn [v] (parser-m. v nil nil)))))

(defn parser
  "Create a parser that always returns a constant value 'v'"
  [v]
  (parser-m. v nil nil))

(defmacro parser-do [bindings expr]
  `(monads.macros/do arrows.proc/parser ~bindings ~expr))

(defn optional
  "Create a parser for an optional element"
  [p]
  (m/plus [p (parser nil)]))

(declare one-or-more)

(defn none-or-more
  "Create a parser for zero or occurances of a given parser"
  [p]
  (optional (one-or-more p)))

(defn one-or-more
  "Create a parser for one or occurances of a given parser"
  [p]
  (parser-do
   [a p
    as (none-or-more p)]
   (cons a as)))

;; a parser that removes the next item from what is being parsed
(def next-item
  (reify
    m/Monad
    (do-result [_ v]
      (parser v))
    (bind [mv f]
      (fn [{:keys [cmd] :as state}]
        (when (seq cmd)
          ((f (first cmd)) (update-in state [:cmd] rest)))))))

;; a parser that returns the entire parser state
(def get-state
  (reify
    clojure.lang.IFn
    (invoke [_ s]
      [s s])

    m/Monad
    (do-result [_ v]
      (parser v))
    (bind [mv f]
      (parser-m. nil mv f))))

(defn get-val
  "Create a parser to get the value associated with 'k' in the parser state"
  [k]
  (m/bind get-state
          (fn [state]
            (parser (get state k)))))

(defn update-state
  "Create a parser that will apply a function to the parser state"
  [f]
  (reify
    clojure.lang.IFn
    (invoke [_ s]
      (let [new-s (f s)]
        [new-s new-s]))

    m/Monad
    (do-result [_ v]
      (parser v))
    (bind [mv f]
      (parser-m. nil mv f))))

(defn set-val
  "Create a parser that set's the value associated with 'k' to 'v'"
  [k v]
  (m/bind (update-state #(assoc % k v))
          #(parser (get % k))))

(defn item-test
  "A parser to test the next item to be parsed and proceed
   only if the result is true."
  [pred]
  (parser-do
   [c next-item
    :when (pred c)]
   c))

(defn is-item
  "A parser to see if the next item to be parsed equals 'item'"
  [item]
  (item-test (partial = item)))

;; A parser to match one binding clause in a proc-do binding block
(def match-binding-clause
  (parser-do
   [bound next-item
    _ (is-item '<-)
    arrow next-item
    _ (is-item '-<)
    expr next-item
    env (get-val :env)
    arr (get-val :arr)
    final-env (set-val :env (conj env bound))]
   `[(arrows/seq (~arr (partial repeat 2))
                 (arrows/par (~arr identity)
                             ~(if (= expr '_)
                                arrow
                                `(arrows/seq (~arr (fn [~(vec env)] ~expr))
                                             ~arrow))))
     (~arr (partial apply conj))]))

;; A parser to match a :let clause in a proc-do binding block
(def match-let-clause
  (parser-do
   [_ (is-item :let)
    bindings next-item
    :let [pairs (partition 2 bindings)
          bound (map first pairs)]
    env (get-val :env)
    arr (get-val :arr)
    _ (set-val :env (concat env bound))]
   `[(arrows/all (~arr identity)
                 (~arr (fn [~(vec env)]
                         (let ~bindings
                           [~@bound]))))
     (~arr (partial apply concat))]))

;; A parser that throws an exception when a binding clause is malformed
(def malformed-clause
  (parser-do
   [cmd (get-val :cmd)
    :when (seq cmd)]
   (throw (Exception. (str "Malformed 'proc-do' expression: " cmd)))))

;; A parser to match any clause in a proc-do binding block
(def match-clause
  (m/plus [match-binding-clause
           match-let-clause
           malformed-clause]))

;; A parser to match all the clauses in a proc-do binding block
(def match-clauses
  (one-or-more match-clause))

;; A parser to match proc-do expression
(def match-do
  (parser-do
   [_ (is-item 'proc-do)
    starting-env (get-val :env)
    arr (get-val :arr)
    bindings next-item
    :let [[bindings {env :env}] (match-clauses {:env starting-env :arr arr
                                                :cmd bindings})]
    final-expr next-item]
   (if (nil? bindings)
     (throw (Exception. "Malformed 'proc-do' expression"))
     `(arrows/seq (~arr vector)
                  ~@(apply concat bindings)
                  (~arr (fn [~(vec env)] ~final-expr))))))

;; A parser to match a proc-if expression
(def match-if
  (parser-do
   [_ (is-item 'proc-if)
    env (get-val :env)
    arr (get-val :arr)
    pred next-item
    true-expr next-item
    false-expr next-item]
   `(arrows/seq (~arr (fn [v#]
                        (let [~@env v#]
                          [~pred v#])))
                (arrows/cond
                 true ~true-expr
                 false ~false-expr))))

;; A parser to match a proc application expression
(def match-application
  (parser-do
   [arr (get-val :arr)
    env (get-val :env)
    arrow next-item
    expr next-item]
   `(arrows/seq (~arr (fn [v#]
                        (let [~@env v#]
                          ~expr)))
                ~arrow)))

;; A parser to match any proc expression
(def match-proc
  (m/plus [match-do
           match-if
           match-application]))
