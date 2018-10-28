(ns sorting.suffix-tree
  (:gen-class)
  (:require [clojure.tools.logging :as log]))

; The only thing a Node has is a suffix-link, so just store it as a vector
(def Nodes (volatile! [-1]))
(def Edges (volatile! {}))

(defn find-edge
  [node c]
  (get @Edges (str node c)))

(declare create-edge)

(defprotocol IEdge
  (edge-insert [this c])
  (split-edge [this suffix st]))

(defrecord Edge [first-char last-char start-node end-node id]
  IEdge
  (edge-insert [{:keys [start-node id] :as this} c]
    (vswap! Edges #(assoc % (str start-node c) (assoc this :id (or id (count @Edges))))))
  (split-edge [this
               {:keys [first-char last-char origin-node] :as suffix}
               s]
    (let [new-edge (create-edge (:first-char this)
                             (+ (:first-char this) (- last-char first-char))
                             origin-node)]
      (vswap! Nodes #(assoc % (:end-node new-edge) origin-node))
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
             (vswap! Nodes #(conj % -1))
             i)
           nil)))

(defprotocol ISuffix
  (explicit? [this])
  (implicit? [this])
  (canonize [this s]))

(defrecord Suffix [origin-node first-char last-char]
  ISuffix
  (explicit? [{:keys [first-char last-char] :as this}] (< last-char first-char))
  (implicit? [_] (complement explicit?))
  (canonize [this s]
    (if (explicit? this)
      this
      (loop [suffix this
             edge (find-edge (:origin-node suffix) (nth s (:first-char suffix)))]
        (let [span (- (:last-char edge) (:first-char edge))]
          (if (> span (- (:last-char suffix) (:first-char suffix)))
            suffix
            (let [new-suffix (assoc suffix
                                    :first-char (+ (:first-char suffix) span 1)
                                    :origin-node (:end-node edge))]
              (if (<= (:first-char new-suffix) (:last-char new-suffix))
                (recur new-suffix (find-edge (:end-node edge) (nth s (:first-char new-suffix))))
                new-suffix))))))))

(defn add-suffix-link
  [last-parent parent]
  (when (> last-parent 0)
    (vswap! Nodes #(assoc % last-parent parent)))
  parent)

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
    (let [parent (:origin-node active-suffix)]
      (if (explicit? active-suffix)
        (if (find-edge (:origin-node active-suffix) (nth s last-char))
          {:last-parent last-parent :parent parent :suffix active-suffix}
          (recur (add-prefix-edge active-suffix
                                  last-char
                                  last-parent
                                  parent
                                  s)))
        (let [edge (find-edge (:origin-node active-suffix) (nth s (:first-char active-suffix)))
              span (- (:last-char active-suffix) (:first-char active-suffix))]
          (if (= (nth s (+ (:first-char edge) span 1)) (nth s last-char))
            {:last-parent last-parent :parent parent :suffix active-suffix}
            (recur (add-prefix-edge active-suffix
                                    last-char
                                    last-parent
                                    (split-edge edge active-suffix s)
                                    s))))))))

(defn add-prefix
  [active-suffix last-char s]
  (let [{:keys [last-parent parent suffix]} (add-prefix-loop active-suffix last-char s)]
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
            (pr-str (subs s (:first-char i) (min (inc (:last-char i)) (count s)))))))

(defn build
  [s]
  (vreset! Nodes [-1])
  (vreset! Edges {})
  (let [suffixed-s (conj (into [] (seq s)) (char 0))]
    (reduce (fn [active i] (add-prefix active i suffixed-s))
            (->Suffix 0 0 -1)
            (range (count suffixed-s)))))

(defn m-lrmus
  [s]
  (build s)
  (let [edges (vals @Edges)
        s-len (inc (count s))
        by-start-node (group-by :start-node
                                (map #(assoc %
                                             :len (inc (- (:last-char %)
                                                          (:first-char %)))) edges))]
    (loop [interior (filter #(not= s-len (:last-char %)) (get by-start-node 0))
           prior-count (count interior)]
      (let [map-fn (fn [i]
                     (if-some [next-level (seq (filter #(not= s-len (:last-char %))
                                                       (get by-start-node (:end-node i))))]
                       (map #(hash-map
                              :last-char (:last-char %)
                              :len (+ (:len i) (:len %))
                              :start-node (:start-node i)
                              :end-node (:end-node %))
                            next-level)
                       [i]))
            new-interior (mapcat map-fn interior)
            new-count (count new-interior)]
        (if (not= interior new-interior)
          (recur new-interior new-count)
          (first (sort-by :sort-by
                       (map #(let [len (inc (:len %))  ; to get the first unique char
                                   last-char  (inc (:last-char %)) ; to include the first unique character
                                   first-char (inc (- last-char len))
                                   s (subs s first-char (+ first-char len))]
                               {:len len
                                :first-char first-char
                                :sort-by (- first-char (* 1024 len))
                                :s s})
                            new-interior))))))))
