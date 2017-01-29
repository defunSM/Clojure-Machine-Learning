(ns clojure-ml.core
  (:use clojure.core.matrix
        [incanter.charts :only [xy-plot add-points scatter-plot histogram]]
        [incanter.core :only [view sel to-matrix]]
        [incanter.stats :only [linear-model sample-normal]]
        [incanter.datasets :only [get-dataset]])
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

(scale A (rand-int 10))

(def A (cl/matrix [[-2 2 3] [-1 1 3] [2 0 -1]]))

(det A)

(defn lmatrix [n]
  (compute-matrix :clatrix [n (+ n 2)]
                  (fn [i j] ({0 -1, 1 2, 2 -1} (- j i) 0))))

(pm (lmatrix 4))

(defn problem
  "Return the map of a problem setup for a given matrix size,
   number of observed values and regularization parameter."
  [n n-observed lambda]
  (let [i (shuffle (range n))]
    {:L (M/* (lmatrix n) lambda)
     :observed (take n-observed i)
     :hidden (drop n-observed i)
     :observed-values (matrix :clatrix
                              (repeatedly n-observed rand))}))

(defn solve
  "Return a map containing the approximated value y of each
   hidden point x."
  [{:keys [L observed hidden observed-values] :as problem}]
  (let [nc (column-count L)
        nr (row-count L)
        L1 (cl/get L (range nr) hidden)
        L2 (cl/get L (range nr) observed)
        l11 (M/* (transpose L1) L1)
        l12 (M/* (transpose L1) L2)]
    (assoc problem :hidden-values
           (M/* -1 (inverse l11) l12 observed-values))))

(defn plot-points
  [s]
  (let [X (concat (:hidden s) (:observed s))
        Y (concat (:hidden-values s) (:observed-values s))]
    (view
     (add-points
      (xy-plot X Y) (:observed s) (:observed-values s)))))

(defn plot-rand-sample []
  (plot-points (solve (problem 150 10 30))))

(plot-rand-sample)

(def X (cl/matrix [8.401 14.475 13.396 12.127 5.044
                   8.339 15.692 17.108 9.253 12.029]))

(def Y (cl/matrix [-1.57 2.32 0.424 0.814 -2.3
                   0.01 1.954 2.296 -0.635 0.328]))

(def linear-samp-scatter
  (scatter-plot X Y))

(defn plot-scatter []
  (view linear-samp-scatter))

(plot-scatter)

(def samp-linear-model
  (linear-model Y X))

(defn plot-model []
  (view (add-points linear-samp-scatter
                    X (:fitted samp-linear-model))))

(plot-model)

(:coefs samp-linear-model)

(:residuals samp-linear-model)

(map (fn [x] (* x x)) (:residuals samp-linear-model))

(reduce + (map (fn [x] (* x x)) (cl/matrix (:residuals samp-linear-model))))

(:sse samp-linear-model)

(view (histogram (sample-normal 1000 :mean 5) :width 700 :height 700))

(sample-normal 50 :mean 5)

(:r-square samp-linear-model)

(def gradient-descent-precision 0.001)

(defn gradient-descent []
  [F' x-start step]
  (loop [x-old x-start]
    (let [x-new (- x-old
                   (* step (F' x-old)))
          dx (- x-new x-old)]
      (if (< dx gradient-descent-precision)
        x-new
        (recur x-new)))))

(def iris
  (to-matrix (get-dataset :iris)))

(def X (sel iris :cols (range 1 5)))
(def Y (sel iris :cols 0))

(def iris-linear-model
  (linear-model Y X))

(:fitted iris-linear-model)

(defn plot-iris-linear-model []
  (let [x (range -100 100)
        y (:fitted iris-linear-model)]
    (view (xy-plot x y :x-label "X" :y-label "Y"))))

(plot-iris-linear-model)

(= (count (:coefs iris-linear-model))
   (+ 1 (column-count X)))

(count (:coefs iris-linear-model))
;; => 5

(defn dist [x y z i j k]
  (let [d1 (- i x)
        d2 (- j y)
        d3 (- k z)]
;;    (sqrt (+ (pow d1 2) (pow d2 2) (pow d3 2)))
    (sqrt (reduce + (map (fn [x] (pow x 2)) [d1 d2 d3])))
    ))

(dist 2 18 19 -1 0 1)
