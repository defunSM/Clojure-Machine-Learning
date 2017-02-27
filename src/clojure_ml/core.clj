(ns clojure-ml.core
  (:use clojure.core.matrix
        [incanter.charts :only [xy-plot add-points scatter-plot histogram]]
        [incanter.core :only [view sel to-matrix bind-columns]]
        [incanter.stats :only [linear-model sample-normal]]
        [incanter.datasets :only [get-dataset]]
        [clojure.pprint :only [pprint]]
        clj-ml.classifiers
        clj-ml.data)
  (:require [clatrix.core :as cl]
            [clojure.core.matrix.operators :as M]))


(def a [-2 -1 -2])
(def b [-5 4 -4])
(def c [-5 5 -2])

(cross (calcvec c a) (calcvec a b))

(def k 9e9)
(def q 35e-9)
(def a 66e-2)
(def b 19e-2)
(def m 4e-3)

(def electric-potential (+ (* (/ (* k q q) a) 2) (/ (* k q q) b)))

(Math/sqrt (/ (* 2 electric-potential) (* 3 m)))

(def d 1.6e-3)
(def E 26)
(def m 9.1e-31)
(def q 1.6e-19)

(def potential-energy (* (/ (Math/pow (/ d 2) 2) 2) q E))

(def pot (* q E d))

(Math/sqrt (/ (* 2 pot) m))

(Math/sqrt (/ (* 2 potential-energy) m))




(cross (calcvec [0 0 0] [4 3 4]) (calcvec [4 3 4] [-3 3 -5]))




(dot [5 -4 1] [4 5 0])
(cross (calcvec a b) [-5 -2 1])


(cross (calcvec [0 0 0] [5 4 2]) (calcvec [0 0 0] [6 -1 1]))

(dot [5 4 2] [6 -1 1])
(def ac [-5 -2 -1])

y



(/ (- (dot [-2 -2 -3] [1 -1 4]) 6) (magnitude [1 -1 4]))

(defn magnitude [[a b c]]
  (Math/sqrt (+ (Math/pow a 2) (Math/pow b 2) (Math/pow c 2))))

(magnitude [1 1 1])

(defn compv [b a]
  (/  (dot a b) (magnitude a)))


(defn calcvec [[a b c] [d e f]]
  (let [i (- d a)
        j (- e b)
        k (- f c)]
    [i j k]))

(def a [3 -2 3])
(def b [6 3 3])
(def c [7 -5 6])
(def d [6 -4 4])

(def ab (calcvec a b))
(def bc (calcvec b c))
(def cd (calcvec c d))
(def da (calcvec d a))

(/ (dot ab (cross bc cd)) 6)

(magnitude )

(dot [-1 5 3] [5 -25 -16])



(dot a (cross b c))


(magnitude (cross ab bc))

(cross [10 7 1] [1 2 1])

(cross pq qr)

(cross [2 -1 1] [-3 -1 -1])

(* 0.5 (magnitude (cross pq qr)))



(* 0.5 (magnitude (cross ab bc)))

(* 0.5 (magnitude (cross ab da)))

(def v [-3 1 5])

(def i [1 0 0])
(def j [0 1 0])
(def k [0 0 1])

(def u [3 0 5])
(def v [-3 4 -5])

(cross u v)

(cross (scale 3 i) (M/+ i j))

(cross (M/+ k j) (M/- k j))

(magnitude v)

(tan (acos (/ (magnitude v) 6)))

(dot c (cross a b))


(dot a (cross a b))

(unitvector a)

(scale (/ 1 (magnitude (cross a b))) (cross a b))

(compv b a)

(defn projv [b a]
  (scale (/ (dot a b) (Math/pow (magnitude a) 2)) a))

(dot [1 2 -1] [-8 -16 8])
(cross [1 2 -1] [-8 -16 8])
(cross [3 9 14] [9 -3 0])
(dot [3 9 14] [9 -3 0])

(dot [8 16 -24] [-24 16 8])
(cross [8 16 -24] [-24 16 8])

(* 2 8 (cos (/ 3.147 4)))

(acos (/ (dot [1 -1 1] [1 1 -1]) (* (magnitude [1 -1 1]) (magnitude [1 1 -1]))))


;; => 9.0

(def f [5 4 5])
(def r [-3 -6 6])

(* (/ (dot f r) (* (magnitude f) (magnitude r))) (magnitude f) 9)

(* 9 (dot f r))

(* 9 (magnitude f))

(magnitude (cross r f))



(projv b a)


(dot [0 4 -5] [4 0 -3])

(def u [-5 4 1])
(def w [-3 5 -2])
(def v [-2, 2, 3])

(dot u w)

(def u [6 2 -3])

(dot u [1 -3 0])

(magnitude [8 -1 -4])
;; => 9.0

(magnitude [2 1 -9])
;; => 9.273618495495704

(acos (/ (dot [8 -1 -4] [2 1 -9]) (* (magnitude [8 -1 -4]) (magnitude [2 1 -9]))))

(def a [3 0 -4])
(def b [1 1 -4])
(def c [4 7 3])

(dot a b)
(dot a c)
(dot b c)

(defn compv [b a]
  (/ (magnitude a) (dot b a)))

(compv [-1 0 4] [1 2 3]) ;; b first a second
;; => 0.34015067152490375
(/ 11 (sqrt 14))

(compv [-1 1 2] [-3 5 10])

(scale (dot u v) u)

(dot (scale (dot w w) u) u)

(+ (dot u v) (dot v w))

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
    (sqrt (+ (pow d1 2) (pow d2 2) (pow d3 2)))
;;    (sqrt (reduce + (map (fn [x] (pow x 2)) [d1 d2 d3])))
    ))

(dist 1 4 7 3 2 9)

(defn linear-model-ols
  [MX MY]
  (let [X (bind-columns (repeat (row-count MX) 1) MX)
        Xt (cl/matrix (transpose X))
        Xt-X (cl/* Xt X)]
    (cl/* (inverse Xt-X) Xt MY)))

(def ols-linear-model
  (linear-model-ols X Y))

(def ols-linear-model-coefs
  (cl/as-vec ols-linear-model))

(cl/as-vec (ols-linear-model X Y))



(+ (/ ne 1.6e-19) 5.3e16)
;; => 3.12500049375E14

(def np 5.3e16)
(def ne (/ (- (/ 7.9e-12 1.6e-19) np) -1))


(- np ne)
;; => 4.9375E7

(/ (- np ne) np)
;; => 9.316037735849057E-10

(def charge 35e-9)
(def d1 21e-2)
(def d2 12e-2)
(def degree (atan (/ d2 d1)))

(def f1 (force charge charge d2))
(def f2 (* (force charge charge (sqrt (+ (pow d1 2) (pow d2 2))))
           (sin degree)))

(+ (- 0 f1) f2)

(to-degrees degree)

(defn force [q1 q2 dis]
  (/ (* 9e9 q1 q2) (pow dis 2)))

(force 0.26e-3 9e-3 9)

(* (sin (atan (/ 9 5))) (force 0.26e-3 9e-3 (sqrt (+ (pow 5 2) (pow 9 2)))))

(def topf (* (sin (atan (/ 9 5))) (force 0.26e-3 9e-3 (sqrt (+ (pow 5 2) (pow 9 2))))))

(def botf (* (sin (atan (/ 9 2.5))) (force 0.26e-3 9e-3 (sqrt (+ (pow 2.5 2) (pow 9 2))))))


(force charge charge d1)

(+ (* (cos degree)
      (force charge charge (sqrt (+ (pow d1 2) (pow d2 2)))))
   (force charge charge d1))
;; => 4.1363043832345583E-4



(/ 9.8 9e9)

(* (/ (* 110 32.1 6.022e23) 1e12) 1.6e-19)

(* (/ (* (/ 110 32.1) 6.022e23) 1e12) 1.6e-19)
;; => 3.3017819314641745E-7

(atan )

(def f1 (force charge charge d2)) ;; top left

(def f2 (force charge charge d1)) ;; bottom right

(def f3 (force charge charge (sqrt (+ (pow d1 2)
                                      (pow d2 2))))) ;; top right

(* (cos degree) f3);; => 9.350310761340328E-5
(* (sin degree) f3);; => 1.636304383234558E-4

(def horizontal (- f2 (* (sin degree) f3)))

horizontal ;; => 1.5649689238659677E-4

(* 4.558276496e-5 (sin (to-radians 60.2551187)))

(defn predict [coefs x]
  {:pre [(= (count coefs)
            (+ 1 (count x)))]}
  (let [x-with-1 (conj x 1)
        products (map * coefs x-with-1)]
    (reduce + products)))

(def xf (map inc))
(transduce xf conj (range 5))

(defn make-sea-bass []
  #{:sea-bass
    (if (< (rand) 0.2) :fat :thin)
    (if (< (rand) 0.7) :long :short)
    (if (< (rand) 0.8) :light :dark)})

(defn make-salmon []
  #{:salmon
    (if (< (rand) 0.8) :fat :thin)
    (if (< (rand) 0.5) :long :short)
    (if (< (rand) 0.3) :light :dark)})

(defn make-sample-fish []
  (if (< (rand) 0.3) (make-sea-bass) (make-salmon)))

(def fish-training-data
  (for [i (range 10000)] (make-sample-fish)))

(defn probability
  [attribute & {:keys
                [category prior-positive prior-negative data]
                :or {category nil
                     data fish-training-data}}]
  (let [by-category (if category
                      (filter category data)
                      data)
        positive (count (filter attribute by-category))
        negative (- (count by-category) positive)
        total (+ positive negative)]
    (float (/ positive total))))

(probability :dark :category :salmon)
(probability :fat :category :salmon)

(* (probability :fat :category :salmon) (probability :dark :category :salmon))
(* (probability :light :category :sea-bass) (probability :thin :category :sea-bass))
(probability :dark :category :sea-bass)
(probability :light :category :salmon)
(probability :light :category :sea-bass)

(defn evidence-of-salmon [& attrs]
  (let [attr-probs (map #(probability % :category :salmon) attrs)
        class-and-attr-prob (conj attr-probs
                                  (probability :salmon))]
    (apply * class-and-attr-prob)))

(defn evidence-of-sea-bass [& attrs]
  (let [attr-probs (map #(probability % :category :sea-bass) attrs)
        class-and-attr-prob (conj attr-probs
                                  (probability :sea-bass))]
    (apply * class-and-attr-prob)))

(defn evidence-of-fish [& attrs]
  (let [attr-probs-of-salmon (map #(probability % :category :salmon) attrs)
        attr-probs-of-sea-bass (map #(probability % :category :sea-bass) attrs)
        class-probs-of-salmon (conj attr-probs-of-salmon (probability :salmon))
        class-probs-of-sea-bass (conj attr-probs-of-sea-bass (probability :sea-bass))
        salmon-prob (apply * class-probs-of-salmon)
        sea-bass-prob (apply * class-probs-of-sea-bass)
        total-prob (+ salmon-prob sea-bass-prob)
        norm-salmon-prob (* 100 (/ salmon-prob total-prob))
        norm-sea-bass-prob (* 100 (/ sea-bass-prob total-prob))]
    (if (< norm-salmon-prob norm-sea-bass-prob)
      {:probability norm-sea-bass-prob :fish :sea-bass :evidence sea-bass-prob}
      {:probability norm-salmon-prob :fish :salmon :evidence salmon-prob})))

(evidence-of-fish :thin)

(evidence-of-sea-bass :dark :long :fat)
;; => 0.0080733078260939
(evidence-of-salmon :dark :long :fat)
;; => 0.19817920769847883

(defn rand-in-range [min max]
  (let [len (- max min)
        rand-len (rand-int len)]
    (+ min rand-len)))

(defn make-sea-bass []
  (vector :sea-bass
        (rand-in-range 6 10)    ;; Length
        (rand-in-range 0 5)     ;; Width
        (rand-in-range 4 10)))  ;; Lightness

(defn make-salmon []
  (vector :salmon
          (rand-in-range 0 7)
          (rand-in-range 4 10)
          (rand-in-range 0 6)))

(defn make-sample-fish []
  (if (< (rand) 0.3) (make-sea-bass) (make-salmon)))

(def fish-training-data
  (for [i (range 10000)] (make-sample-fish)))

(def bayes-classifier (make-classifier :bayes :naive))

(def fish-template
  [{:category [:salmon :sea-bass]}
   :length :width :lightness])

(def fish-data-set
  (make-dataset "fish" fish-template fish-training-data))

(defn train-bayes-classifier []
  (dataset-set-class fish-data-set 0)
  (classifier-train bayes-classifier fish-data-set))

(train-bayes-classifier)

(def sample-fish
  (make-instance fish-data-set [:salmon 5 3 1]))

(classifier-classify bayes-classifier sample-fish)

(def K1-classifier (make-classifier :lazy :ibk {:num-neighbors 10}))

(defn train-K1-classifier []
  (dataset-set-class fish-data-set 0)
  (classifier-train K1-classifier fish-data-set))

(train-K1-classifier)

(classifier-classify K1-classifier sample-fish)

(def DT-classifier (make-classifier :decision-tree :c45))

(defn train-DT-classifier []
  (dataset-set-class fish-data-set 0)
  (classifier-train DT-classifier fish-data-set))

(println (train-DT-classifier))

(classifier-classify DT-classifier sample-fish)


;; Vector stuff

(defn addvec [op x y]
  (map (fn [x y] (op x y)) x y))

(defn magnitude [[a b c]]
  (Math/sqrt (+ (Math/pow a 2) (Math/pow b 2) (Math/pow c 2))))

(defn unitvector [[a b c]]
  (let [total (Math/sqrt (+ (Math/pow a 2)
                            (Math/pow b 2)
                            (Math/pow c 2)))
        a1 (/ a total)
        b1 (/ b total)
        c1 (/ c total)]
    [a1 b1 c1]))



(def v1 [-1 -4 -4])
(def v2 [-2 -3 5])

(def arrays [[-8 10 8]
             [-8 6 8]
             [0 0 -20]
             [4 -16 20]
             [24 -30 -24]
             [9 9 0]])

(def arrays2 [[-12 16 -16]
              [5 -10 -15]
              [-15 -6 6]
              [-48 -60 -24]
              [-16 -20 -8]
              [6 -12 -3]])

(def u [3 1 -1])
(def v [0 -4 -1])
(def w [3 0 -2])

(magnitude (scale (/ 1 (magnitude w)) w)) ;; always makes 1

(def setofa (map unitvector arrays))
;; => ([-0.5298129428260175 0.6622661785325219 0.5298129428260175]
;; [-0.6246950475544243 0.4685212856658182 0.6246950475544243]
;; [0.0 0.0 -1.0]
;; [0.1543033499620919 -0.6172133998483676 0.7715167498104595]
;; [0.5298129428260175 -0.6622661785325219 -0.5298129428260175]
;; [0.7071067811865476 0.7071067811865476 0.0]
(while (> x 5)
  ())

(map unitvector arrays2)
;; => ([-0.4685212856658182 0.6246950475544243 -0.6246950475544243]
;; [0.2672612419124244 -0.5345224838248488 -0.8017837257372731]
;; [-0.8703882797784892 -0.3481553119113957 0.3481553119113957]
;; [-0.5962847939999439 -0.7453559924999299 -0.29814239699997197]
;; [-0.5962847939999439 -0.7453559924999299 -0.29814239699997197] [0.4364357804719847 -0.8728715609439694 -0.21821789023599236])

(def u [2 5 -4])
(def v [4 1 4])

(scale 7 u)
(scale (/ -1 3) v)
(addvec - (scale 5 u) (scale 6 v))
(addvec - v u)

(magnitude v1)

(addvec + [-1 -4 -4] [-2 -3 5])
;; => (-3 -7 1)
(addvec - [-1 -4 -4] [-2 -3 5])
;; => (1 -1 -9)
(scale 2 v1)
;; => (-2 -8 -8)
(addvec + (scale 3 v1) (scale 4 v2))
;; => (-11 -24 8)

(* 2 3.14 9e9 2.1e-4)

(* (* 2 3.14 9e9 2.1e-4) (- 1 (/ 1 (Math/sqrt (+ (Math/pow (/ 0.23 0.15) 2) 1)))))

(* (- (/ 1 (Math/sqrt (+ (Math/pow (/ 0.087 0.15) 2) 1)))
      (/ 1 (Math/sqrt (+ (Math/pow (/ 0.23 0.15) 2) 1))))
   (* 2 3.14 9e9 2.1e-4))

(Math/pow 12.5e-2 3)
(Math/pow 6e-2 3)

(Math/pow 12.5e-2 3)
(Math/pow 6e-2 3)

(/ (- (Math/pow 12.5e-2 3) (Math/pow 6e-2 3)) 3)

(* 6e-3)
