(ns arrows
  (:require [arrows.proc :refer :all]
            [arrows.vis :refer :all])
  (:refer-clojure :exclude [seq nth cond condp identity]))

;; basic arrow protocol
(defprotocol Arrow
  (arrow-arr [_ f])
  (arrow-seq [p ps])
  (arrow-nth [p n]))

;; effecient parallel protocol
(defprotocol ArrowPar
  (arrow-par [p ps]))

;; arrow with choice protocol
(defprotocol ArrowChoice
  (arrow-select [_ vp-pairs]))

;; identity arrow protocol
(defprotocol ArrowIdentity
  (arrow-identity [_]))

(defn set-attr [p & kv-pairs]
  (set-attr* p kv-pairs))

(defn seq
  "Create an arrow that is a sequential composition of the
   given arrows."
  [& [p & ps]]
  (arrow-seq p ps))

(defn nth
  "Create an arrow that accepts one parameter as input which is
   a sequence of values. The nth value is passed through the
   given arrow and the result replaces that value in the input."
  [n p]
  (arrow-nth p n))

(defn par
  "Create an arrow that accepts one parameter as input which is
   a collection of values. Passes each value through the respective
   arrow, returning a vector of all the results. The number of
   values in the input must equal the number of arrows."
  [p & ps]
  (arrow-par p ps))

(defn all
  "Create an arrow that will send its input value to all the given
   arrows in parallel. The output value will be a vector of all
   the results."
  [p & ps]
  (seq (arrow-arr p #(repeat (inc (count ps)) %))
       (arrow-par p ps)))

(defn identity
  "Create an identity arrow of the same type as 'p'. This arrow
   passes all input values through unchanged."
  [p]
  (arrow-identity p))

(defn cond
  "Create an arrow that accepts a pair as input. The first value
   of the pair determines which arrow the second value of the pair
   is routed to. 'vp-pairs' is a sequence of value/arrow pairs."
  [& vp-pairs]
  (arrow-select (second vp-pairs) vp-pairs))

(defn condp
  "Creates an arrow that applies 'pred' to the incoming value and
   then routes the value to the correct arrow based on the result."
  [pred & vp-pairs]
  (seq
   (all pred (arrow-identity pred))
   (apply cond vp-pairs)))

;; TODO: figure out looping arrows
#_(defn loop [p initial-value & [fb-p]]
    (arrow-loop p [initial-value fb-p]))

;; defining the default arrow-par behavior
;; gets overridden when the arrow-par protocol is defined
(extend-type Object
  ArrowPar
  (arrow-par [p ps]
    (let [ps (cons p ps)]
      (apply seq (map #(arrow-nth %1 %2)
                      ps
                      (range 0 (count ps)))))))

(defmacro proc
  "Provide an easier notation to define arrow values"
  [arr-fn apat cmd]
  (-> {:env (vec apat) :cmd cmd :arr arr-fn}
      match-proc
      first))

(defmacro defproc
  "Provide an easier notation to define arrow values and
   assign them to symbols."
  ([name proc]
     `(def ~name (arrows.vis/label ~proc (str '~name))))
  ([name arr apat cmd]
     `(def ~name (arrows.vis/label (proc ~arr ~apat ~cmd) (str '~name)))))
