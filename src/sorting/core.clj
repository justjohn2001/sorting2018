(ns sorting.core
  (:gen-class)
  (:require [sorting.suffix-tree :as tree]
            [clojure.java.io :as io]))

(defn -main
  [input output]
  (let [to-sort (with-open [rdr (clojure.java.io/reader input)]
                  (doall (line-seq rdr)))]
    
    (defn extract [d] (nth to-sort (nth d 3)))
    
    (defn lrmu-sort [coll]
      (let [compares [- - compare (fn [idx-1 idx-2] (compare (nth coll idx-1) (nth coll idx-2)))]]
        (map extract
             (sort (fn [xx yy] (or (some #(when (not= 0 %) %)
                                         (map #(%1 %2 %3) compares xx yy))
                                   0))
                   (map-indexed (fn [idx s] (tree/m-lrmus s idx)) coll)))))
    
    (lrmu-sort to-sort)

    (Thread/sleep 5000)

    (def start (System/currentTimeMillis))
    (def sorted (lrmu-sort to-sort))
    (def end (System/currentTimeMillis))
    
    (println (- end start))

    (spit output (str (clojure.string/join "\n" sorted) "\n"))))
