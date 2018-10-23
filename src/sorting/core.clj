(ns sorting.core
  (:gen-class)
  (:require [sorting.suffix-tree :as tree]))

(defn -main
  [input output]
  (let [to-sort (with-open [rdr (clojure.java.io/reader input)]
                  (doall (line-seq rdr)))]
    (my-sort to-sort)
    (def start (System/currentTimeMillis))
    (def sorted (my-sort to-sort))
    (println (- end (System/currentTimeMillis)))
    (dorun (map #(spit output % :append true) sorted))))


