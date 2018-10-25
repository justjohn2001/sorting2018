(ns sorting.sort
  (:require [sorting.suffix-tree :as tree]))

(defn lrmu-compare [origionals]
  (fn [[cnt-1 lidx-1 lrmu-1 idx-1]
       [cnt-2 lidx-2 lrmu-2 idx-2]]
    (let [cnt (- cnt-1 cnt-2)]
      (if (= 0 cnt)
        (let [lidx (- lidx-1 lidx-2)]
          (if (= 0 lidx)
            (let [lrmu (compare lidx-1 lidx-2)]
              (if (= 0 lrmu)
                (compare (nth origionals idx-1) (nth origionals idx-2))
                lrmu))
            lidx))
        cnt))))

(defn lrmu-sort [coll]
  (map #(nth coll (nth % 3))
       (sort (lrmu-compare coll)
             (map-indexed (fn [idx s] (conj (tree/m-lrmus s) idx)) coll))))
