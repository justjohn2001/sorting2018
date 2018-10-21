(ns sorting.core
  (:gen-class))

(defrecord Node [children suffix-link start end suffix-index])

(defrecord State [active-length active-edge active-node remaining-suffix-count last-new-node])
; active-node-list is a vector of chars to reach the active node

(def leaf-end (atom 0))

(defn new-node
  [start end]
  {:suffix-link []
   :start start
   :end end
   :suffix-index -1})

(defn edge-length
  [n]
  (inc (- (:end n) (:start n))))

(defn walk-down
  [tree
   {:keys [active-length active-edge active-node] :as state}
   curr-node]
  (let [curr-len (edge-length (get-in tree curr-node))]
    (when (< curr-len active-length)
      (assoc state
             :active-edge (+ active-edge curr-len)
             :active-length (- active-length curr-len)
             :active-node curr-node))))

(defn mutate-tree
  [tree
   {:keys [active-length active-edge active-node last-new-node] :as state}]
  (let [next-node (conj active-node c)]
    (if (nil? (get-in tree next-node))
      ; Extension Rule 2
      (let [new-tree (-> tree
                         (assoc-in next-node (new-node n leaf-end))
                         (as-> <nt>
                             (if last-new-node
                               (assoc-in <nt>
                                         (conj last-new-node :suffix-link)
                                         active-node)
                               <nt>)))]
        (recur (dec remaining-suffix-count)
               new-tree
               state
               nil))
      ; else There is an outgoing edge
      (if-let [new-state (walk-down state next-node)]
        ; Start from next node
        (recur remaining-suffix-count tree new-state last-new-node)
        ; Extension Rule 3
        (if (= c (nth text (+ active-length (get-in tree (conj next-node :start)))))
          ; APCFER3
          {:tree (if (and last-new-node (seq active-node))
                   (assoc-in tree (conj last-new-node :suffix-link) active-node)
                   tree)
           :state (assoc state :last-new-node nil :active-length (inc active-length))}
          ; Extension Rule 2
          (let [split-end (delay (+ (get-in tree (conj next-node :start))
                                    active-length
                                    -1))
                start (get-in tree (conj next-node :start))
                split (assoc (new-node start split-end)
                             c
                             (update (get-in tree next-node)
                                     :start
                                     (partial + active-length)))])   ))

      ))

(defn extend-suffix-tree
  [state text n]
  (swap! leaf-end (fn [_] n))
  (loop [remaining-suffix-count (inc (:remaining-suffix-count state))
         tree' tree
         state' state
         last-new-node nil]
    (if (zero? remaining-suffix-count')
      (assoc state'
             :remaining-suffix-count remaining-suffix-count
             :tree tree')
      (let [{:keys [active-edge active-length active-node]} state'
            active-edge (if (zero? active-length) pos active-edge)
            c (int (nth text n))]
        (recur (dec remaining-suffix-count')
               ())))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(let [t (transient {:a 1 :b [2 3 4]})
      b (transient (:b t))]
  (assoc! t :b (persistent! (conj! b 5)))
  (persistent! t))
