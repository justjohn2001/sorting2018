(ns sorting.core
  (:gen-class)
  (:require [sorting.suffix-tree :as tree]))

(defn -main
  [& args]
  (tree/build "bananas"))


