(ns sorting.suffix-tree
  (:gen-class))

(def Nodes (atom [-1]))
(def Edges (atom {}))

(defn find-edge
  [node c]
  (printf "Finding %s-%s\n" node c)
  (get @Edges (str node c)))

(declare create-edge)

(defprotocol IEdge
  (edge-insert [this c])
  (split-edge [this suffix st]))

(defrecord Edge [first-char last-char start-node end-node id]
  IEdge
  (edge-insert [{:keys [start-node id] :as this} c]
    (printf "Insert %d%c\n" start-node c)
    (swap! Edges #(assoc % (str start-node c) (assoc this :id (or id (count @Edges)))))
    (printf "Keys %s\n" (keys @Edges)))
  (split-edge [this
               {:keys [first-char last-char origin-node] :as suffix}
               s]
    (printf "Splitting edge - %s with %s\n" this suffix)
    (let [new-edge (create-edge (:first-char this)
                             (+ (:first-char this) (- last-char first-char))
                             origin-node)]
      (swap! Nodes #(assoc % (:end-node new-edge) origin-node))
      (edge-insert new-edge (nth s (:first-char new-edge)))
      (let [new-start (+ (:first-char this) (- last-char first-char) 1)]
        (edge-insert (assoc this
                            :first-char new-start
                            :start-node (:end-node new-edge))
                     (nth s new-start)))
      (:end-node new-edge))))

(defn create-edge
  ([] (map->Edge {:start-node -1}))
  ([first-char last-char parent-node]
   (->Edge first-char
           last-char
           parent-node
           (let [i (count @Nodes)]
             (when (> i 100) (throw (ex-info "Too many nodes" {:i i})))
             (printf "Adding node %d\n" i)
             (swap! Nodes #(conj % -1))
             i)
           nil)))

(defprotocol ISuffix
  (explicit? [this])
  (implicit? [this])
  (canonize [this s]))

(defrecord Suffix [origin-node first-char last-char]
  ISuffix
  (explicit? [{:keys [last-char first-char] :as this}] (< last-char first-char))
  (implicit? [_] (complement explicit?))
  (canonize [{:keys [first-char last-char origin-node] :as this} s]
    (printf "Canonizing %s\n" (pr-str this))
    (printf "Edges - %s\n" (keys @Edges))
    (if (explicit? this)
      this
      (loop [suffix this
             edge (find-edge origin-node (nth s first-char))]
        (printf "Edge - %s\n" (pr-str edge))
        (let [span (- (:last-char edge) (:first-char edge))]
          (if (> span (- (:last-char suffix) (:first-char suffix)))
            suffix
            (let [new-suffix (assoc suffix
                                    :first-char (+ (:first-char suffix) span 1)
                                    :origin-node (:end-node edge))]
              (when (<= (:first-char new-suffix) (:last-char new-suffix))
                (recur new-suffix (find-edge (:end-node edge) (nth s (:first-char new-suffix))))))))))))

(defn add-suffix-link
  [last-parent parent]
  (when (> last-parent 0)
    (swap! Nodes #(assoc % last-parent parent)))
  parent)

#_(defn add-prefix
  [next-active-suffix last-char s]
  (printf "add-prefix %s %s %s\n" next-active-suffix last-char s)
  (loop [last-parent-node -1
         active-suffix' next-active-suffix]
    (printf "looping %s %s\n" last-parent-node (pr-str active-suffix'))
    (let [add-prefix-step-2 (fn [parent-node]
                              (printf "add-prefix-step-2 last - %s parent - %s\n" last-parent-node parent-node)
                              (let [new-edge (create-edge last-char (count s) parent-node)]
                                (edge-insert new-edge (nth s last-char)))
                              [(add-suffix-link last-parent-node parent-node)
                               (canonize (if (zero? (:origin-node active-suffix'))
                                           (update active-suffix' :first-char inc)
                                           (update active-suffix' :origin-node #(nth @Nodes %)))
                                         s)])
          parent-node (:origin-node active-suffix')]
      (if (explicit? active-suffix')
        (if (find-edge (:origin-node active-suffix') (nth s last-char))
          (do (add-suffix-link last-parent-node parent-node)
              (canonize (update active-suffix' :last-char inc) s))
          (do
            (println "Explicit, not found")
            (let [[new-parent new-suffix]
                  (add-prefix-step-2 (:origin-node active-suffix'))]
              (printf "Rcuring with %s\n" new-parent)
              (recur new-parent new-suffix))))
        (let [edge (find-edge (:origin-node active-suffix') (nth s (:first-char active-suffix')))
              span (- (:last-char active-suffix') (:first-char active-suffix'))]
          (printf "Implicit - %s\n" edge)
          (if (= (nth s (+ (:first-char edge) span 1))
                 (nth s last-char))
            (do (add-suffix-link last-parent-node parent-node)
                (canonize (update active-suffix' :last-char inc) s))
            (let [[new-parent new-suffix]
                  (add-prefix-step-2 (split-edge edge active-suffix' s))]
              (recur new-parent new-suffix))))))))

(defn add-prefix-edge
  [active-suffix last-char last-parent parent s]
  (let [edge (create-edge last-char (count s) parent)
        new-parent (add-suffix-link last-parent parent)
        new-suffix (if (zero? (:origin-node active-suffix))
                     (update active-suffix :first-char inc)
                     (update active-suffix :origin-node #(nth @Nodes %)))]
    (edge-insert edge (nth s last-char))
    {:active-suffix (canonize new-suffix s)
     :last-parent (add-suffix-link last-parent parent)}))

(defn add-prefix-loop
  [active-suffix last-char s]
  (loop [{:keys [active-suffix last-parent]}
         {:active-suffix active-suffix
          :last-parent -1}]
    (printf "add-prefix-loop %s %s %s\n" active-suffix last-char last-parent)
    (let [parent (:origin-node active-suffix)]
      (if (explicit? active-suffix)
        (do (println "Explicit")
            (if (find-edge (:origin-node active-suffix) (nth s last-char))
              {:last-parent last-parent :parent parent :suffix active-suffix}
              (do (println "Recur - Edge Not Found")
                  (recur (add-prefix-edge active-suffix
                                          last-char
                                          last-parent
                                          parent
                                          s)))))
        (do (println "Implicit")
            (let [edge (find-edge (:origin-node active-suffix) (nth s (:first-char active-suffix)))
                  span (- (:last-char active-suffix) (:first-char active-suffix))]
              (if (= (nth s (+ (:first-char edge) span 1)) (nth s last-char))
                {:last-parent last-parent :parent parent :suffix active-suffix}
                (do (println "Recur - char doesn't match")
                    (recur (add-prefix-edge active-suffix
                                            last-char
                                            last-parent
                                            (split-edge edge active-suffix s)
                                            s))))
              ))))))

(defn add-prefix
  [active-suffix last-char s]
  (printf "Starting %s %s\n" (pr-str active-suffix) last-char)
  (let [{:keys [last-parent parent suffix]} (add-prefix-loop active-suffix last-char s)]
    (printf "Finishing %s %s" (pr-str active-suffix) last-char)
    (add-suffix-link last-parent parent)
    (canonize (update suffix :last-char inc) s)))

(defn dump-edges
  [s]
  (println "Dumping edges")
  (println "   id start   end suf first last")
  (doseq [i (sort-by :id (vals @Edges))]
    (printf "%5d %5s %5s %3s %5s %6s  %s\n"
            (:id i)
            (:start-node i)
            (:end-node i)
            (nth @Nodes (:end-node i))
            (:first-char i)
            (:last-char i)
            (subs s (:first-char i) (min (inc (:last-char i)) (count s))))
    ))

(defn build
  [s]
  (reset! Nodes [-1])
  (reset! Edges {})
  (prn s)
  (let [suffixed-s (str s "\0")]
    (reduce (fn [active i] (add-prefix active i suffixed-s))
            (->Suffix 0 0 -1)
            (range (count suffixed-s)))
    (dump-edges suffixed-s))
  )



