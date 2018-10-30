(ns sorting.core
  (:gen-class)
  (:require [sorting.suffix-tree :as tree]
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

;; load everything in order skipping -main

(defn lrmu-sort [coll]
  (let [compares [- - compare (fn [idx-1 idx-2] (compare (nth coll idx-1) (nth coll idx-2)))]]
    (sort (fn [xx yy] (or (some #(when (not= 0 %) %)
                                (map #(%1 %2 %3) compares xx yy))
                          0))
          (map-indexed (fn [idx s] (tree/m-lrmus s idx)) coll))))

(def data (with-open [rdr (clojure.java.io/reader (io/resource "data1.txt"))]
            (doall (line-seq rdr))))

(defn extract [d] (nth data (nth d 3)))

;; We are shooting for 3 seconds ... or at least less than 6.
;; On my macbook, peter's code took 3 seconds.
(time (def sorted (doall (map extract (lrmu-sort data)))))

;; sorted data to compare to
(def sorted-data (with-open [rdr (clojure.java.io/reader (io/resource "out1.txt"))]
                   (doall (line-seq rdr))))

(= sorted sorted-data)

#_(def parsed (time (doall (map-indexed (fn [idx s] (tree/m-lrmus s idx)) data))))

;; Run me
(time (spit "out1.txt" (str (clojure.string/join "\n" sorted) "\n")))
;; Then
;; diff out1.txt resources/out1.txt
;; should be clear

;;todo warn of reflection
