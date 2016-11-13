(ns clojure-ml.core
  (:use clojure.core.matrix)
  (:require [clatrix.core :as cl]
            [clojure.core.matrix.operators :as M]))

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

(def a (cl/matrix [[0 1 2] [3 4 5]]))

(cl/set a 1 1 0)

(cl/map-indexed (fn [i j m] i) a)

(defn square-mat
  "Creates a square matrix of size n x n
whose elements are all e"

  [n e & {:keys [implementation]
          :or {implementation :persistent-vector}}]
  (let [repeater #(repeat n %)]
    (matrix implementation (-> e repeater repeater))))

(square-mat 5 2 :implementation :clatrix)

(defn id-mat
  "Creates an identity matrix of n x n size."
  [n]
  (let [init (square-mat n 0 :implementation :clatrix)
        identity-f (fn [i j n]
                     (if (= i j) 1 n))]
    (cl/map-indexed identity-f init)))

(pm (identity-matrix 5))


;; doesn't generate the random matrix you are expecting.
(defn rand-square-mat
  "Generates a random matrix."
  [n]
  (matrix (repeat n (repeat n (rand-int 100)))))

(rand-square-mat 4)

(defn rand-square-clmat
  [n]
  (cl/map (rand-int 100) (square-mat n 100 :implmentation :clatrix)))

(rand-square-clmat 5)

(defn rand-square-mat
  "Creates a n x n matrix filling with a random number up to r."
  [n r]
  (matrix
   (repeatedly n #(map rand-int (repeat n r)))))

(rand-square-mat 5)

(cl/rnorm 10 25 10 10)

(defn id-computed-mat
  [n]
  (compute-matrix [n n] #(if (= %1 %2) 1 0)))

(pm (id-computed-mat 5))

(defn rand-computed-mat
  [n m]
  (compute-matrix [n m]
                  (fn [i j] (rand-int 100))))

(rand-computed-mat 3 10)

(def A (matrix [[0 1 2] [3 4 5]]))

(def B (matrix [[0 0 0] [0 0 0]]))

(M/== B A)

(def C (M/+ A B))

(defn mat-add

  "Add two matrices"
  ([A B]
   (mapv #(mapv + %1 %2) A B))
  ([A B & more]
   (let [M (concat [A B] more)]
     (reduce mat-add M))))

(mat-add A B A)

(concat [A B])

(def A (matrix [1 2 3 4 5 6]))

(def B (matrix [10 20 20 30 30 40]))

(def C (matrix [11 12 13 14]))

(def N 10)

(pm (M/* A B))

(scale B (rand-int 10))
