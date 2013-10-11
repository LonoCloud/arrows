(ns arrows.test.vis
  (:require [clojure.test :refer :all]
            [clojure.zip :as z]
            [arrows.vis :refer :all]
            [arrows :as a]))

(deftype id-arr [fun meta-data]
  clojure.lang.IDeref
  (deref [_]
    [fun meta-data])

  clojure.lang.IMeta
  (meta [_]
    meta-data)

  clojure.lang.IFn
  (invoke [_ x]
    (fun x))

  a/Arrow
  (arrow-arr [_ f]
    (id-arr. f {:op :arrow-arr
                :info f}))

  (arrow-seq [p ps]
    (if (= (count ps) 0)
      p
      (let [cp (a/arrow-seq (first ps) (rest ps))]
        (id-arr. (comp cp p)
                 {:op :arrow-seq
                  :args (cons p ps)}))))

  (arrow-nth [p n]
    (id-arr. (fn [xs]
               (let [xs (vec xs)]
                 (assoc xs n (p (nth xs n)))))
             {:op :arrow-nth
              :args [p n]}))

  a/ArrowChoice
  (arrow-select [_ vp-pairs]
    (let [vp-pairs (apply hash-map vp-pairs)]
      (id-arr. (fn [[x y]]
                 (let [p (or (get vp-pairs x)
                             (get vp-pairs :else))]
                   (p y)))
               {:op :arrow-select
                :args (into {} vp-pairs)})))

  a/ArrowPar
  (arrow-par [p ps]
    (id-arr. (fn [x] x)
             {:op :arrow-par
              :args (cons p ps)}))

  a/ArrowIdentity
  (arrow-identity [_]
    (id-arr. identity {:op :arrow-arr
                       :label "identity"
                       :info 'identity}))

  arrows.vis/ArrowVis
  (set-attr* [p kv-pairs]
    (let [[f m] (deref p)]
      (id-arr. f (apply assoc m kv-pairs))))
  (op [p]
    (:op (meta p)))
  (args [p]
    (:args (meta p)))
  (label [p lbl]
    (let [[f m] (deref p)]
      (id-arr. f (assoc m :label lbl)))))

;; all f's should just be keywords
(defn arr [f & [meta]]
  (id-arr. f (merge {:op :arrow-arr :info f :label (str f)} meta)))

(deftest test-entry-node
  (is (= {:id :entry-node}
         (entry-node {:id :entry-node})))
  (is (= {:id :entry-node}
         (entry-node {:id :outer
                      :entry :mezz
                      :mezz {:id :mezz
                             :entry :entry-node
                             :entry-node {:id :entry-node}}}))))

(deftest test-path-to-exit-edges
  (is (= [:next]
         (path-to-exit-edges {:id :exit-node})))
  (is (= [:mezz :inner :next]
         (path-to-exit-edges {:id :outer
                              :entry :dont-care
                              :exit :mezz
                              :mezz {:id :mezz
                                     :entry :dont-care
                                     :exit :inner
                                     :inner {:id :inner}}}))))

(deftest test-add-exit-edge
  (is (= {:id :exit-node
          :next [{:to :next-node :label "next"}]}
         (add-exit-edge {:id :exit-node}
                        {:to :next-node :label "next"})))
  (is (= {:id :outer
          :entry :dont-care
          :exit :mezz
          :mezz {:id :mezz
                 :entry :dont-care
                 :exit :inner
                 :inner {:id :inner
                         :next [{:to :next-node :label "next"}]}}}
         (add-exit-edge {:id :outer
                         :entry :dont-care
                         :exit :mezz
                         :mezz {:id :mezz
                                :entry :dont-care
                                :exit :inner
                                :inner {:id :inner}}}
                        {:to :next-node :label "next"}))))

(defn normal-node [[id dg] children]
  (let [entry-node (->> children (remove nil?) first second)
        entry-id (or (:entry entry-node) (:label entry-node))]
    [(:label dg) (-> dg
                     (select-keys [:label])
                     (into children)
                     (assoc :entry entry-id
                            :exit (first (last children))))]))

(defn normalize-dg [dg]
  (let [root (z/zipper branch? children normal-node [(:id dg) dg])
        norm-root (loop [loc root]
                    (if (z/end? loc)
                      (z/root loc)
                      (-> loc
                          (z/edit (fn [[id dg]]
                                    (when dg
                                      (let [dg (if (subgraph? dg)
                                                 dg
                                                 (update-in dg [:next]
                                                            (fn [nxt]
                                                              (if nxt
                                                                (map (fn [edg]
                                                                       (dissoc edg :to))
                                                                     nxt)))))]
                                        [(:label dg) (dissoc dg :id)]))))
                          (z/next)
                          (recur))))]
    (second norm-root)))

(deftest test-add-exit-edge
  (let [dg (append-dg (proc-to-dg (arr :a))
                      (proc-to-dg (arr :b)))
        entry-id (:entry dg)
        exit-id (:exit dg)]
    (is (= {:id (:id dg)
            :entry entry-id
            :exit exit-id
            entry-id {:next [{:to exit-id}]
                        :id entry-id
                        :label ":a"
                        :terminate nil}
            exit-id {:next [{:to :next-node
                             :label "to next node"}]
                        :id exit-id
                        :label ":b"
                        :terminate nil}}
           (add-exit-edge dg {:to :next-node
                              :label "to next node"})))))

(deftest test-remove-entry
  (is (= nil
         (remove-entry {:entry :node
                        :exit :node
                        :node {}})))
  (is (= {:entry :nnode
          :exit :xnode
          :xnode {}}
         (remove-entry {:entry :nnode
                        :nnode {}
                        :exit :xnode
                        :xnode {}})))
  (is (= nil
         (remove-entry {:entry :subdg
                        :subdg {:entry :node
                                :exit :node
                                :node {}}
                        :exit :subdg})))
  (is (= {:entry :subdg
          :exit :subdg
          :subdg {:id :subdg
                  :entry :nnode
                  :exit :xnode
                  :xnode {}}}
         (remove-entry {:entry :subdg
                        :exit :subdg
                        :subdg {:id :subdg
                                :entry :nnode
                                :nnode {}
                                :exit :xnode
                                :xnode {}}})))
  (is (= {:entry :ndg
          :exit :top-node
          :top-node {}
          :ndg nil}
         (remove-entry {:entry :ndg
                        :exit :top-node
                        :top-node {}
                        :ndg {:id :ndg
                              :entry :nnode
                              :nnode {}
                              :exit :nnode}})))
  (is (= {:entry :ndg
          :exit :top-node
          :top-node {}
          :ndg {:id :ndg
                :entry :nnode
                :exit :xnode
                :xnode {}}}
         (remove-entry {:entry :ndg
                        :exit :top-node
                        :top-node {}
                        :ndg {:id :ndg
                              :entry :nnode
                              :nnode {}
                              :exit :xnode
                              :xnode {}}}))))

(deftest test-append-dg
  (let [entry-dg {:id (genkey)}
        exit-dg {:id (genkey)}
        dg (append-dg entry-dg exit-dg "forward")]
    (is (= {:id (:id dg)
            :entry (:id entry-dg)
            :exit (:id exit-dg)
            (:id entry-dg) (assoc entry-dg
                             :next [{:to (:id (entry-node exit-dg))
                                     :label "forward"}])
            (:id exit-dg) exit-dg}
           dg)))
  (let [entry-dg (append-dg (proc-to-dg (arr :a))
                            (proc-to-dg (arr :b)))
        entry-id (:entry entry-dg)
        exit-id (:exit entry-dg)
        next-id (genkey)
        exit-dg {:id next-id :next [{:to :final}]}
        dg (append-dg entry-dg exit-dg "edge label")]
    (is (= {:id (:id dg)
            :entry (:id entry-dg)
            :exit next-id
            (:id entry-dg) {:id (:id entry-dg)
                            :entry entry-id
                            :exit exit-id
                            entry-id {:next [{:to exit-id}]
                                      :id entry-id
                                      :label ":a"
                                      :terminate nil}
                            exit-id {:next [{:to next-id
                                             :label "edge label"}]
                                     :id exit-id
                                     :label ":b"
                                     :terminate nil}}
            next-id exit-dg}
           dg)))
  (let [entry-dg {:id (genkey)}
        exit-dg (append-dg (proc-to-dg (arr :a))
                           (proc-to-dg (arr :b)))
        dg (append-dg entry-dg exit-dg "forward")]
    (is (= {:id (:id dg)
            :entry (:id entry-dg)
            :exit (:id exit-dg)
            (:id entry-dg) {:id (:id entry-dg)
                            :next [{:to (:entry exit-dg)
                                    :label "forward"}]}
            (:id exit-dg) exit-dg}
           dg)))
  (let [entry-dg (append-dg (proc-to-dg (arr :a))
                            (proc-to-dg (arr :b)))
        exit-dg (append-dg (proc-to-dg (arr :c))
                           (proc-to-dg (arr :d)))
        dg (append-dg entry-dg exit-dg "forward")]
    (is (= {:id (:id dg)
            :entry (:id entry-dg)
            :exit (:id exit-dg)
            (:id entry-dg) (add-exit-edge entry-dg {:to (:entry exit-dg)
                                                    :label "forward"})
            (:id exit-dg) exit-dg}
           dg)))
  (let [entry-dg {:id (genkey)}
        dg (append-dg entry-dg
                      {:id (genkey) :next [{:to :next-node}]})]
    (is (= {:id (:id dg)
            :next [{:to :next-node}]}
           dg)))
  (let [first-dg (proc-to-dg (arr :a))
        second-dg (proc-to-dg (arr :b))
        third-dg (proc-to-dg (arr :c))
        dg (-> first-dg
               (append-dg second-dg)
               (append-dg third-dg))]
    (is (= {:id (:id dg)
            :entry (:id first-dg)
            :exit (:id third-dg)
            (:id first-dg) (assoc first-dg
                             :next [{:to (:id second-dg)}])
            (:id second-dg) (assoc second-dg
                              :next [{:to (:id third-dg)}])
            (:id third-dg) third-dg}
           dg)))
  (let [entry-dg (append-dg (proc-to-dg (arr :a))
                            (proc-to-dg (arr :b)))
        exit-dg (append-dg (proc-to-dg (arr :c))
                           (proc-to-dg (arr :d)))
        dg (append-dg entry-dg exit-dg)]
    (is (= (-> (set-exits entry-dg [{:to (:id (entry-node exit-dg))}])
               (merge (subnodes exit-dg))
               (assoc :id (:id dg)
                      :exit (:exit exit-dg)))
           dg)))
  (let [exit-dg (assoc (append-dg (proc-to-dg (arr :c))
                                  (proc-to-dg (arr :d)))
                  :label "subgraph")
        entry-dg (append-dg (proc-to-dg (arr :a))
                            (proc-to-dg (arr :b)))
        dg (append-dg entry-dg exit-dg)]
    (is (= (-> entry-dg
               (set-exits [{:to (:id (entry-node exit-dg))}])
               (merge {:id (:id dg)
                       (:id exit-dg) exit-dg
                       :exit (:id exit-dg)}))
           dg)))
  (let [entry-dg (-> (append-dg (proc-to-dg (arr :a))
                                (proc-to-dg (arr :b)))
                     (assoc :label "subgraph"))
        exit-dg (append-dg (proc-to-dg (arr :c))
                           (proc-to-dg (arr :d)))
        dg (append-dg entry-dg exit-dg)]
    (is (= (assoc exit-dg
             :id (:id dg)
             :entry (:id entry-dg)
             (:id entry-dg) (set-exits entry-dg [{:to (:id (entry-node exit-dg))}]))
           dg)))
  (let [exit-dg (-> (append-dg (proc-to-dg (arr :c))
                               (proc-to-dg (arr :d)))
                    (assoc :label "subgraph2"))
        entry-dg (-> (proc-to-dg (arr :a))
                     (append-dg (proc-to-dg (arr :b)))
                     (assoc :label "subgraph1"))
        dg (append-dg entry-dg exit-dg)]
    (is (= {:id (:id dg)
            :entry (:id entry-dg)
            :exit (:id exit-dg)
            (:id exit-dg) exit-dg
            (:id entry-dg) (set-exits entry-dg [{:to (:id (entry-node exit-dg))}])}
           dg)))
  (let [exit-dg (-> (append-dg {:id (genkey)}
                               (proc-to-dg (arr :d)))
                    (assoc :label "subgraph2"))
        entry-dg {:id (genkey)}
        dg (append-dg entry-dg exit-dg)]
    (is (= (assoc exit-dg :id (:id dg))
           dg)))
  (let [final-dg (proc-to-dg (arr :d))
        bridge-dg {:id (genkey) :next [{:to (:id final-dg)}]}
        exit-dg {:id (genkey)
                 :entry (:id bridge-dg)
                 :exit (:id final-dg)
                 (:id bridge-dg) bridge-dg
                 (:id final-dg) final-dg}
        entry-dg (-> (append-dg (proc-to-dg (arr :a))
                                (proc-to-dg (arr :b)))
                     (assoc :label "subgraph"))
        dg (append-dg entry-dg exit-dg)]
    (is (= {:id (:id dg)
            (:id entry-dg) (set-exits entry-dg [{:to (:id final-dg)}])
            :entry (:id entry-dg)
            :exit (:id final-dg)
            (:id final-dg) final-dg}
           dg)))
  (let [final-dg {:id (genkey) :next [{:to :next-node}]}
        exit-dg {:id (genkey)
                 :entry (:id final-dg)
                 :exit (:id final-dg)
                 (:id final-dg) final-dg}
        entry-dg (-> (append-dg (proc-to-dg (arr :a))
                                (proc-to-dg (arr :b)))
                     (assoc :label "subgraph"))
        dg (append-dg entry-dg exit-dg)]
    (is (= (-> entry-dg
               (assoc :id (:id dg))
               (set-exits (:next final-dg)))
           dg)))
  (let [final-dg (proc-to-dg (arr :d))
        bridge-dg {:id (genkey) :next [{:to (:id final-dg)}]}
        exit-dg {:id (genkey)
                 :entry (:id bridge-dg)
                 :exit (:id final-dg)
                 (:id bridge-dg) bridge-dg
                 (:id final-dg) final-dg}
        entry-dg (append-dg (proc-to-dg (arr :a))
                            (proc-to-dg (arr :b)))
        dg (append-dg entry-dg exit-dg)]
    (is (= (assoc (set-exits entry-dg (:next bridge-dg))
             :id (:id dg)
             :exit (:id final-dg)
             (:id final-dg) final-dg)
           dg)))
  (let [final-dg {:id (genkey) :next [{:to :next-node}]}
        exit-dg {:id (genkey)
                 :entry (:id final-dg)
                 :exit (:id final-dg)
                 (:id final-dg) final-dg}
        entry-dg (append-dg (proc-to-dg (arr :a))
                            (proc-to-dg (arr :b)))
        dg (append-dg entry-dg exit-dg)]
    (is (= (-> entry-dg
               (assoc :id (:id dg))
               (set-exits (:next final-dg)))
           dg)))
  (let [exit-dg {:id (genkey) :next [{:to :next-node}]}
        entry-dg (assoc (append-dg (proc-to-dg (arr :a))
                                   (proc-to-dg (arr :b)))
                   :label "subgraph")
        dg (append-dg entry-dg exit-dg)]
    (is (= (-> entry-dg
               (assoc :id (:id dg))
               (set-exits (:next exit-dg)))
           dg)))
  (let [exit-dg {:id (genkey) :next [{:to :next-node}]}
        entry-dg (append-dg (proc-to-dg (arr :a))
                            (proc-to-dg (arr :b)))
        dg (append-dg entry-dg exit-dg)]
    (is (= (-> entry-dg
               (assoc :id (:id dg))
               (set-exits (:next exit-dg)))
           dg)))
  (let [final-dg (proc-to-dg (arr :c))
        bridge-dg {:id (genkey) :next [{:to (:id final-dg)}]}
        exit-dg {:entry (:id bridge-dg)
                 :exit (:id final-dg)
                 :label "subgraph"
                 :id (genkey)
                 (:id bridge-dg) bridge-dg
                 (:id final-dg) final-dg}
        entry-dg (proc-to-dg (arr :a))
        dg (append-dg entry-dg exit-dg)]
    (is (= {:id (:id dg)
            :entry (:id entry-dg)
            :exit (:id exit-dg)
            (:id entry-dg) (set-exits entry-dg [{:to (:id final-dg)}])
            (:id exit-dg) {:entry (:id bridge-dg)
                           :exit (:id final-dg)
                           :label "subgraph"
                           :id (:id exit-dg)
                           (:id final-dg) final-dg}}
           dg)))
  (let [bridge-dg {:id (genkey) :next [{:to :next-node}]}
        exit-dg {:entry (:id bridge-dg)
                 :exit (:id bridge-dg)
                 :label "subgraph"
                 :id (genkey)
                 (:id bridge-dg) bridge-dg}
        entry-dg (proc-to-dg (arr :a))
        dg (append-dg entry-dg exit-dg)]
    (is (= {:id (:id dg)
            :entry (:id entry-dg)
            :exit (:id exit-dg)
            (:id entry-dg) (set-exits entry-dg [{:to (:id bridge-dg)}])
            (:id exit-dg) {:entry (:id bridge-dg)
                           :exit (:id bridge-dg)
                           :label "subgraph"
                           :id (:id exit-dg)
                           (:id bridge-dg) bridge-dg}}
           dg)))
  (let [exit-dg (assoc (append-dg (proc-to-dg (arr :a))
                                  (proc-to-dg (arr :b)))
                  :label "subgraph")
        entry-dg {:id (genkey)}
        dg (append-dg entry-dg exit-dg)]
    (is (= (assoc exit-dg :id (:id dg))
           dg)))
  (let [exit-dg (append-dg (proc-to-dg (arr :a))
                           (proc-to-dg (arr :b)))
        entry-dg (proc-to-dg (arr :x))
        dg (append-dg entry-dg exit-dg)]
    (is (= {:id (:id dg)
            :entry (:id entry-dg)
            (:id entry-dg) (set-exits entry-dg [{:to (:id (entry-node exit-dg))}])
            :exit (:id exit-dg)
            (:id exit-dg) exit-dg}
           dg)))
  (let [exit-dg (append-dg (proc-to-dg (arr :a))
                           (proc-to-dg (arr :b)))
        entry-dg {:id (genkey)}
        dg (append-dg entry-dg exit-dg)]
    (is (= (assoc exit-dg :id (:id dg))
           dg)))
  (let [entry-dg (assoc (append-dg (proc-to-dg (arr :a))
                                   (proc-to-dg (arr :b)))
                   :label "subgraph")
        exit-dg (proc-to-dg (arr :x))
        dg (append-dg entry-dg exit-dg)]
    (is (= {:id (:id dg)
            :entry (:id entry-dg)
            (:id entry-dg) (add-exit-edge entry-dg {:to (:id exit-dg)})
            :exit (:id exit-dg)
            (:id exit-dg) exit-dg}
           dg)))
  (let [exit-dg (assoc (append-dg (proc-to-dg (arr :a))
                                  (proc-to-dg (arr :b)))
                  :label "subgraph")
        entry-dg (proc-to-dg (arr :x))
        dg (append-dg entry-dg exit-dg)]
    (is (= {:id (:id dg)
            :entry (:id entry-dg)
            (:id entry-dg) (add-exit-edge entry-dg {:to (:id (entry-node exit-dg))})
            :exit (:id exit-dg)
            (:id exit-dg) exit-dg}
           dg))))

(deftest test-arr-visualization
  (is (= {:label ":a" :terminate nil}
         (dissoc (proc-to-dg (arr :a)) :id)))
  (is (= {:label ":a" :terminate true}
         (-> (arr :a)
             (a/set-attr :terminate true)
             (proc-to-dg)
             (dissoc :id))))
  (is (not= "" (proc-to-dot (arr :a)))))

(deftest test-seq-to-dg
  (let [dg (proc-to-dg (a/seq (arr :a) (arr :b)))]
    (is (= ":a" (->> (:entry dg)
                   (get dg)
                   :label)))
    (is (= ":b" (->> (:entry dg)
                   (get dg)
                   :next
                   first
                   :to
                   (get dg)
                   :label)))
    (is (= ":b" (->> (:exit dg)
                   (get dg)
                   :label)))))

(deftest test-proc-to-dg-nth
  (let [dg (proc-to-dg (a/nth 5 (arr :a)))]
    (is (or (= ":a" (->> (:entry dg)
                       (get dg)
                       :next
                       first
                       :to
                       (get dg)
                       :label))
            (= ":a" (->> (:entry dg)
                       (get dg)
                       :next
                       second
                       :to
                       (get dg)
                       :label))))))

(a/defproc one (arr :a {:label "estarto"}))
(a/defproc two
  (a/condp (arr :t {:label "test"})
           true (a/seq (arr :a {:label "true branch"})
                       (arr :x))
           false (a/seq (arr :a {:label "false branch"})
                        (arr :y))))
(a/defproc three (arr :final {:label "finito"}))

(defn do-while [test-p body-p]
  (let [loop-dg #(let [body-dg (proc-to-dg body-p)
                       exit-id (genkey)
                       test-dg (add-exit-edge (proc-to-dg test-p)
                                              {:to exit-id
                                               :label "false"})]
                   (-> (append-dg test-dg body-dg "true")
                       (add-exit-edge {:to (entry-id test-dg)})
                       (assoc :exit exit-id
                              exit-id {:id exit-id})))]
    (arr :do-while
         {:op :do-while :args body-p
          :proc-to-dg loop-dg})))

(def proc
  (do-while (arr :outer-test)
            (a/seq (arr :x)
                   (label
                    (do-while (arr :inner-test)
                              (a/proc arr [input]
                                      (proc-do
                                       [a <- (arr :a {:label "make a"}) -< input
                                        b <- two -< input
                                        c <- (arr :c {:label "combine a & b"}) -< [a b]]
                                       c)))
                    "inner-loop"))))

#_(spit "doc/arr.dot"
      (str "digraph g {\n"
           "label = \"(arr :a)\""
           (proc-to-dot (arr :a))
           "}"))

#_(spit "doc/seq.dot"
      (str "digraph \"Arrow Examples\" {\nrankdir=LR\n"
           "subgraph cluster_seq {\n"
           "label = \"(a/seq (arr :a) (arr :b))\""
           (proc-to-dot (a/seq (arr :a) (arr :b)))
           "}\n}"))

#_(spit "doc/nth.dot"
      (str "digraph \"Arrow Examples\" {\nrankdir=LR\n"
           "subgraph cluster_nth {\n"
           "label = \"(a/nth 3 (arr :a))\""
           (proc-to-dot (a/nth 3 (arr :a)))
           "}\n}"))

#_(spit "doc/par.dot"
      (str "digraph \"Arrow Examples\" {\nrankdir=LR\n"
           "subgraph cluster_par {\n"
           "label = \"(a/par (arr :a) (arr :b))\""
           (proc-to-dot (a/par (arr :a) (arr :b)))
           "}\n}"))

#_(spit "doc/all.dot"
      (str "digraph \"Arrow Examples\" {\nrankdir=LR\n"
           "subgraph cluster_all {\n"
           "label = \"(a/all (arr :a) (arr :b))\""
           (proc-to-dot (a/all (arr :a) (arr :b)))
           "}\n}"))

#_(spit "doc/select.dot"
      (str "digraph \"Arrow Examples\" {\nrankdir=LR\n"
           "subgraph cluster_select {\n"
           "label = \"(a/cond :a (arr :a) :b (arr :b))\""
           (proc-to-dot (a/cond :a (arr :a) :b (arr :b)))
           "}\n}"))

#_(spit "zoo.dot"
      (str "digraph \"Arrow Examples\" {\nrankdir=LR\n"
           "subgraph cluster_arr {\n"
           "labeljust=\"l\""
           "label = \"(arr :a)\""
           (proc-to-dot (arr :a))
           "}\n"
           "subgraph cluster_seq {\n"
           "labeljust=\"l\""
           "label = \"(a/seq (arr :a) (arr :b))\""
           (proc-to-dot (a/seq (arr :a) (arr :b)))
           "}\n"
           "subgraph cluster_nth {\n"
           "labeljust=\"l\""
           "label = \"(a/nth 3 (arr :a))\""
           (proc-to-dot (a/nth 3 (arr :a)))
           "}\n"
           "subgraph cluster_par {\n"
           "labeljust=\"l\""
           "label = \"(a/par (arr :a) (arr :b))\""
           (proc-to-dot (a/par (arr :a) (arr :b)))
           "}\n"
           "subgraph cluster_all {\n"
           "labeljust=\"l\""
           "label = \"(a/all (arr :a) (arr :b))\""
           (proc-to-dot (a/all (arr :a) (arr :b)))
           "}\n"
           "subgraph cluster_cond {\n"
           "labeljust=\"l\""
           "label = \"(a/cond :a (arr :a) :b (arr :b))\""
           (proc-to-dot (a/cond :a (arr :a) :b (arr :b)))
           "}\n}"))