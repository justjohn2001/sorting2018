(ns sorting.sort
  (:require [sorting.suffix-tree :as tree]))

(defn compare-full [origionals idx-1 idx-2]
  (compare (nth origionals idx-1) (nth origionals idx-2)))

(defn compare-lrmu [x y]
  (compare x y))

#_(defn lrmu-compare [origionals]
  (fn [[cnt-1 lidx-1 lrmu-1 idx-1 :as xx]
       [cnt-2 lidx-2 lrmu-2 idx-2 :as yy]]
    (or (some #(when (not= 0 %) %)
              (map #(%1 %2 %3) [- - compare-lrmu compare-full] xx yy))
        0)
    (println (lazy-seq '((- cnt-1 cnt-2)
                          (- lidx-1 lidx-2)
                          (compare-lrmu lrmu-1 lrmu-2)
                          (compare-full origionals idx-1 idx-2))))
    (or (some #(when (not= 0 %) %)
              (lazy-seq '((- cnt-1 cnt-2)
                          (- lidx-1 lidx-2)
                          (compare-lrmu lrmu-1 lrmu-2)
                          (compare-full origionals idx-1 idx-2))))
        0)
    #_(let [cnt (- cnt-1 cnt-2)]
      (if (= 0 cnt)
        (let [lidx (- lidx-1 lidx-2)]
          (if (= 0 lidx)
            (let [lrmu (compare-lrmu lidx-1 lidx-2)]
              (if (= 0 lrmu)
                (compare-full origionals idx-1 idx-2)
                #_(compare (nth origionals idx-1) (nth origionals idx-2))
                lrmu))
            lidx))
        cnt))))

(defn lrmu-sort [coll]
  (let [compares [- - compare (fn [idx-1 idx-2] (compare (nth coll idx-1) (nth coll idx-2)))]]
    (sort (fn [xx yy] (or (some #(when (not= 0 %) %)
                                (map #(%1 %2 %3) compares xx yy))
                          0))
          (map-indexed (fn [idx s] (conj (tree/m-lrmus s) idx)) coll))))

(compare "a" "b")

