;; generate a graphviz DOT file from an arrow expression
(ns arrows.vis
  (:require [clojure.zip :as z]))

(defprotocol ArrowVis
  (set-attr* [p kv-pairs])
  (label [p lbl])
  (op [p])
  (sub-procs [p])
  (args [p]))

(defn genkey []
  (keyword (str (gensym "node_"))))

(defn subgraph? [dg]
  (contains? dg :entry))

(defn subnodes [dg]
  (dissoc dg :label :id :entry :exit :terminate))

(defn path-to-entry-node
  ([dg] (path-to-entry-node dg []))
  ([dg path]
     (if (not (subgraph? dg))
       path
       (recur (get dg (:entry dg)) (conj path (:entry dg))))))

(defn entry-node [dg]
  (let [path (path-to-entry-node dg)]
    (if (empty? path)
      dg
      (get-in dg path))))

(defn entry-id [dg]
  (:id (entry-node dg)))

(defn path-to-exit-edges
  ([dg] (path-to-exit-edges dg []))
  ([dg path]
     (if (not (subgraph? dg))
       (conj path :next)
       (recur (get dg (:exit dg)) (conj path (:exit dg))))))

(defn set-exits [dg exit-edges]
  (assoc-in dg (path-to-exit-edges dg) exit-edges))

(defn add-exit-edge [dg exit-edge]
  (update-in dg (path-to-exit-edges dg) conj exit-edge))

(defn append-exit-edges [dg exit-edges]
  (update-in dg (path-to-exit-edges dg) concat exit-edges))

(defn exit-edges [dg]
  (get-in dg (path-to-exit-edges dg)))

(defn blank-label? [lbl]
  (or (nil? lbl) (= "" lbl)))

(defn remove-entry [dg]
  (if (subgraph? (get dg (:entry dg)))
    (let [new-entry (remove-entry (get dg (:entry dg)))]
      (if (and (nil? new-entry)
               (= (:entry dg) (:exit dg)))
        nil
        (assoc dg (:entry dg) new-entry)))
    (if (= (:entry dg) (:exit dg))
      nil
      (dissoc dg (:entry dg)))))

(defn append-dg
  ([entry-dg exit-dg] (append-dg entry-dg exit-dg nil))
  ([entry-dg exit-dg lbl]
     (assoc (cond
             (not (blank-label? lbl))
             {:entry (:id entry-dg)
              :exit (:id exit-dg)
              (:id entry-dg) (add-exit-edge entry-dg {:to (:id (entry-node exit-dg))
                                                      :label lbl})
              (:id exit-dg) exit-dg}

             (and (not (subgraph? entry-dg))
                  (blank-label? (:label entry-dg)))
             exit-dg

             (and (subgraph? entry-dg)
                  (not (subgraph? exit-dg))
                  (blank-label? (:label exit-dg)))
             (append-exit-edges entry-dg (exit-edges exit-dg))

             (and (subgraph? entry-dg)
                  (not (blank-label? (:label entry-dg)))
                  (subgraph? exit-dg)
                  (not (blank-label? (:label exit-dg))))
             {:entry (:id entry-dg)
              :exit (:id exit-dg)
              (:id entry-dg) (add-exit-edge entry-dg {:to (:id (entry-node exit-dg))})
              (:id exit-dg) exit-dg}

             (and (subgraph? entry-dg)
                  (blank-label? (:label entry-dg))
                  (subgraph? exit-dg)
                  (not (blank-label? (:label exit-dg))))
             (-> entry-dg
                 (add-exit-edge {:to (:id (entry-node exit-dg))})
                 (assoc :exit (:id exit-dg)
                        (:id exit-dg) exit-dg))

             (and (subgraph? entry-dg)
                  (not (blank-label? (:label entry-dg)))
                  (subgraph? exit-dg)
                  (blank-label? (:label exit-dg))
                  (not (blank-label? (:label (entry-node exit-dg)))))
             (assoc exit-dg
               :entry (:id entry-dg)
               (:id entry-dg) (add-exit-edge entry-dg
                                             {:to (:id (entry-node exit-dg))}))

             (and (subgraph? entry-dg)
                  (blank-label? (:label entry-dg))
                  (subgraph? exit-dg)
                  (blank-label? (:label exit-dg))
                  (not (blank-label? (:label (entry-node exit-dg)))))
             (-> entry-dg
                 (add-exit-edge {:to (:id (entry-node exit-dg))})
                 (merge (subnodes exit-dg))
                 (assoc :exit (:exit exit-dg)))

             (and (subgraph? entry-dg)
                  (not (blank-label? (:label entry-dg)))
                  (not (subgraph? exit-dg))
                  (not (blank-label? (:label exit-dg))))
             {:entry (:id entry-dg)
              :exit (:id exit-dg)
              (:id entry-dg) (add-exit-edge entry-dg {:to (:id exit-dg)})
              (:id exit-dg) exit-dg}

             (and (subgraph? entry-dg)
                  (blank-label? (:label entry-dg))
                  (not (subgraph? exit-dg))
                  (not (blank-label? (:label exit-dg))))
             (-> entry-dg
                 (add-exit-edge {:to (:id exit-dg)})
                 (assoc :exit (:id exit-dg)
                        (:id exit-dg) exit-dg))

             (and (not (subgraph? entry-dg))
                  (not (blank-label? (:label entry-dg)))
                  (subgraph? exit-dg)
                  (not (blank-label? (:label exit-dg)))
                  (not (blank-label? (:label (entry-node exit-dg)))))
             {:entry (:id entry-dg)
              (:id entry-dg) (add-exit-edge entry-dg {:to (:id (entry-node exit-dg))})
              :exit (:id exit-dg)
              (:id exit-dg) exit-dg}

             (and (subgraph? entry-dg)
                  (not (blank-label? (:label entry-dg)))
                  (subgraph? exit-dg)
                  (blank-label? (:label exit-dg))
                  (blank-label? (:label (entry-node exit-dg))))
             (let [new-exit (remove-entry exit-dg)]
               (if (nil? new-exit)
                 (->> exit-dg
                      (entry-node)
                      (exit-edges)
                      (append-exit-edges entry-dg))
                 (assoc new-exit :entry (:id entry-dg)
                        (:id entry-dg) (->> exit-dg
                                            (entry-node)
                                            (exit-edges)
                                            (append-exit-edges entry-dg)))))

             (and (subgraph? entry-dg)
                  (blank-label? (:label entry-dg))
                  (subgraph? exit-dg)
                  (blank-label? (:label exit-dg))
                  (blank-label? (:label (entry-node exit-dg))))
             (let [new-exit (remove-entry exit-dg)]
               (if (nil? new-exit)
                 (append-exit-edges entry-dg (exit-edges exit-dg))
                 (-> entry-dg
                     (append-exit-edges (->> exit-dg
                                             (entry-node)
                                             (exit-edges)))
                     (merge (subnodes new-exit))
                     (assoc :exit (:exit exit-dg)))))

             (and (not (subgraph? entry-dg))
                  (not (blank-label? (:label entry-dg)))
                  (subgraph? exit-dg)
                  (not (blank-label? (:label exit-dg)))
                  (blank-label? (:label (entry-node exit-dg))))
             (let [new-exit (remove-entry exit-dg)]
               (if (nil? new-exit)
                 {:entry (:id entry-dg)
                  (:id entry-dg) (add-exit-edge entry-dg {:to (:id (entry-node exit-dg))})
                  :exit (:id exit-dg)
                  (:id exit-dg) exit-dg}
                 {:entry (:id entry-dg)
                  (:id entry-dg) (append-exit-edges entry-dg (exit-edges (entry-node exit-dg)))
                  :exit (:id exit-dg)
                  (:id exit-dg) new-exit}))

             (and (not (subgraph? entry-dg))
                  (not (blank-label? (:label entry-dg)))
                  (subgraph? exit-dg)
                  (blank-label? (:label exit-dg))
                  (blank-label? (:label (entry-node exit-dg))))
             (let [new-exit (remove-entry exit-dg)]
               (if (nil? new-exit)
                 entry-dg
                 {:entry (:id entry-dg)
                  (:id entry-dg) (append-exit-edges entry-dg (exit-edges (entry-node exit-dg)))
                  :exit (:id exit-dg)
                  (:id exit-dg) new-exit}))

             (and (not (subgraph? entry-dg))
                  (not (blank-label? (:label entry-dg)))
                  (subgraph? exit-dg)
                  (blank-label? (:label exit-dg))
                  (not (blank-label? (:label (entry-node exit-dg)))))
             {:entry (:id entry-dg)
              (:id entry-dg) (add-exit-edge entry-dg {:to (:id (entry-node exit-dg))})
              :exit (:id exit-dg)
              (:id exit-dg) exit-dg}

             (and (not (subgraph? entry-dg))
                  (not (blank-label? (:label entry-dg)))
                  (not (subgraph? exit-dg))
                  (not (blank-label? (:label exit-dg))))
             {:entry (:id entry-dg)
              :exit (:id exit-dg)
              (:id entry-dg) (add-exit-edge entry-dg
                                            {:to (:id (entry-node exit-dg))})
              (:id exit-dg) exit-dg}

             (and (not (subgraph? entry-dg))
                  (not (subgraph? exit-dg))
                  (blank-label? (:label exit-dg)))
             (set-exits entry-dg (exit-edges exit-dg))

             :else
             (do
               (throw (Exception. "Missed a case"))))
       :id (genkey))))

(declare proc-to-dg)

(defn arr-to-dg [p]
  {:label (get (meta p) :label)
   :terminate (get (meta p) :terminate)})

(defn nth-to-dg [p]
  (let [args (get (meta p) :args)
        entry (genkey)
        exit (genkey)
        sub-dg (proc-to-dg (first args))]
    {:label (get (meta p) :label)
     :entry entry
     :exit exit
     entry {:id entry
            :next [{:to (entry-id sub-dg)
                    :label (format "(nth v %d)" (second args))}
                   {:to exit :label "v"}]}
     (:id sub-dg) (add-exit-edge sub-dg {:to exit :label "x"})
     exit {:id exit
           :label (format "(assoc v %d x)" (second args))}}))

(defn par-to-dg [p]
  (let [entry (genkey)
        exit (genkey)
        branches (map proc-to-dg
                      (get (meta p) :args))
        g (reduce (fn [dg branch-dg]
                    (assoc dg
                      (:id branch-dg) (add-exit-edge branch-dg {:to exit})))
                  {}
                  branches)]
    (assoc g
      :label (get (meta p) :label)
      :entry entry
      :exit exit
      entry {:id entry
             :next (map-indexed (fn [n dg]
                                  (when (nil? (entry-id dg))
                                    (throw (Exception. "BOGUS")))
                                  {:to (entry-id dg)
                                   :label (format "(nth v %s)" n)})
                                branches)}
      exit {:id exit :label "vector"})))

(defn select-to-dg [p]
  (let [entry (genkey)
        exit (genkey)
        branches (into {} (map (fn [[v p]]
                                 [v (proc-to-dg p)])
                               (get (meta p) :args)))
        g (reduce (fn [dg [v branch-dg]]
                    (assoc dg
                      (:id branch-dg) (add-exit-edge branch-dg {:to exit})))
                  {}
                  branches)]
    (assoc g
      :label (get (meta p) :label)
      :entry entry
      :exit exit
      entry {:id entry
             :next (map (fn [[v p]]
                          (when (nil? (entry-id p))
                            (throw (Exception. "BOGUS")))
                          {:to (entry-id p)
                           :label (str v)})
                        branches)
             :label "(select (first x))"}
      exit {:id exit :label ""})))

(defn proc-to-dg [p]
  (let [custom-dg-fn (get (meta p) :proc-to-dg)
        dg (if custom-dg-fn
             (custom-dg-fn)
             (condp = (op p)
               :arrow-arr (arr-to-dg p)
               :arrow-seq (assoc (->> (args p)
                                      (map proc-to-dg)
                                      (reduce append-dg))
                            :label (get (meta p) :label))
               :arrow-nth (nth-to-dg p)
               :arrow-par (par-to-dg p)
               :arrow-select (select-to-dg p)
               :arrow-identity {}))]
    (assoc dg :id (genkey))))

(defn branch? [[id dg]]
  (subgraph? dg))

(defn children [[id dg]]
  (concat [[(:entry dg) (get dg (:entry dg))]]
          (->> (dissoc dg :label :id :entry :exit :terminate
                       (:entry dg) (:exit dg))
               (seq)
               (sort-by first))
          (if (not= (:exit dg) (:entry dg))
            [[(:exit dg) (get dg (:exit dg))]]
            [])))

(defn dot-edge [from {:keys [to label]}]
  (format "%s -> %s%s"
          (name from) (name to)
          (if label
            (format " [label=\"%s\"];\n" label)
            ";\n")))

(defn node-to-dot [siblings node]
  (let [{internal true
         external false} (group-by #(contains? siblings (:to %)) (:next node))
         _ (when (nil? (:id node))
             (throw (Exception. "INCONCEIVABLE!!")))
         node-str (if (:terminate node)
                    (format "%s [label=\"%s\"];\n" (name (:id node))
                            (:label node ""))
                    (format "%s [label=\"%s\"];\n%s\n" (name (:id node))
                            (:label node "")
                            (->> internal
                                 (map (partial dot-edge (:id node)))
                                 (apply str))))]
    ;; TODO: refactor
    (if (:terminate node)
      {:body node-str}
      {:body node-str
       :exits (map (partial dot-edge (:id node)) external)})))

(defn make-node [[id node] children]
  (let [{subgraph :body
         sub-exits :exits} (->> children
                                (map second)
                                (reduce (fn [acc {:keys [body exits]}]
                                          (-> acc
                                              (update-in [:body] str body)
                                              (update-in [:exits] concat exits)))))]
    [id {:body (if (or (nil? (:label node))
                       (= "" (:label node)))
                 (apply str \newline subgraph \newline sub-exits)
                 (format "subgraph cluster_%s {\nlabeljust=\"l\"\nlabel = \"%s\"\n%s}\n%s"
                         (name (:id node))
                         (:label node)
                         subgraph
                         (apply str sub-exits)))}]))

(defn dg-to-dot [dg]
  (let [root (z/zipper branch? children make-node [(:id dg) dg])
        norm-root (loop [loc root]
                    (let [siblings (into (set (map first (z/lefts loc)))
                                         (map first (z/rights loc)))]
                      (if (z/end? loc)
                        (z/root loc)
                        (-> loc
                            (z/edit (fn [[id dg]]
                                      (when dg
                                        (if (subgraph? dg)
                                          [id dg]
                                          [id (node-to-dot siblings dg)]))))
                            (z/next)
                            (recur)))))]
    (second norm-root)))

(defn proc-to-dot [p]
  (let [dg (proc-to-dg p)]
    (->> dg
         (dg-to-dot)
         :body
         (str (format "subgraph start {\nstart [label = \"start\"];}\nstart -> %s;\n"
                      (name (entry-id dg)))))))
