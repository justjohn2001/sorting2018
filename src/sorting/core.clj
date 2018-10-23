(ns sorting.core
  (:gen-class)
  (:require [sorting.suffix-tree :as tree]))

(defn -main
  [input output]
  (let [to-sort (with-open [rdr (clojure.java.io/reader input)]
                  (doall (line-seq rdr)))

        my-sort (fn [coll]
                  (map #(to-sort (first %))
                       (sort (fn [[idx-1 [cnt-1 lidx-1 lrmu-1]]
                                  [idx-2 [cnt-2 lidx-2 lrmu-2]]]
                               (let [cnt (- cnt-1 cnt-2)]
                                 (if (= 0 cnt)
                                   (let [lidx (- lidx-1 lidx-2)]
                                     (if (= 0 lidx)
                                       (let [lrmu (compare lindex-1 lindex-2)]
                                         (if (= 0 lrmu)
                                           (compare (get origionals idx-1) (get origionals idx-2))
                                           lrmu))
                                       idx))
                                   cnt)))
                             (map-indexed parse coll))))]
    (my-sort to-sort)
    (def start (System/currentTimeMillis))
    (def sorted (my-sort to-sort))
    (println (- end (System/currentTimeMillis)))
    (dorun (map #(spit output % :append true) sorted))))


