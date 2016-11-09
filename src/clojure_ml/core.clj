(ns clojure-ml.core
  (:use clojure.core.matrix)
  (:require [clatrix.core :as cl]))

(matrix [[0 1 2] [3 4 5]])

(def a (matrix [[0 1 2] [3 4 5]]))

(pm a)

(def b (cl/matrix [[0 1 2] [3 4 5]]))

(matrix :persistent-vector [[1 2] [2 1]])

(matrix :clatrix [[1 2] [2 1]])

(matrix? b)

(cl/clatrix? a)

(count (cl/matrix [0 1 2]))

(row-count (cl/matrix [0 1 2]))

(column-count (cl/matrix [0 1 2]))
