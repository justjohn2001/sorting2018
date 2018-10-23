(ns sorting.sort
  (:require [sorting.suffix-tree :as tree]))

(defn lrmu-compare [origionals]
  (fn [[idx-1 [cnt-1 lidx-1 lrmu-1]]
       [idx-2 [cnt-2 lidx-2 lrmu-2]]]
    (let [cnt (- cnt-1 cnt-2)]
      (if (= 0 cnt)
        (let [lidx (- lidx-1 lidx-2)]
          (if (= 0 lidx)
            (let [lrmu (compare lidx-1 lidx-2)]
              (if (= 0 lrmu)
                (compare (get origionals idx-1) (get origionals idx-2))
                lrmu))
            idx))
        cnt))))

(defn lrmu-sort [coll]
  (map #(to-sort (first %))
       (sort (lrmu-compare coll)
        (map-indexed parse coll))))
