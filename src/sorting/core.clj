(ns sorting.core
  (:gen-class)
  (:require [sorting.suffix-tree :as tree]
            [sorting.sort :as s]
            [clojure.java.io :as io]))

(defn -main
  [input output]
  (let [to-sort (with-open [rdr (clojure.java.io/reader input)]
                  (doall (line-seq rdr)))]
    (my-sort to-sort)
    (def start (System/currentTimeMillis))
    (def sorted (my-sort to-sort))
    (println (- end (System/currentTimeMillis)))
    (dorun (map #(spit output % :append true) sorted))))


(def data (with-open [rdr (clojure.java.io/reader (io/resource "data1.txt"))]
            (doall (line-seq rdr))))

(time (def sorted (doall (s/lrmu-sort data))))

(time (dorun (map #(spit "output.txt" % :append true) sorted)))
