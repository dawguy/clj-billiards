(ns clj-billiards.vector)

(defn dot [v-a v-b]
  (+ (* (:x v-a) (:x v-b))
     (* (:y v-a) (:y v-b))))
(defn magnitude [v]
  (Math/sqrt (+ (Math/pow (:x v) 2)
                (Math/pow (:y v) 2))))
(defn subtract [v-a v-b]
  {:x (- (:x v-a) (:x v-b))
   :y (- (:y v-a) (:y v-b))})
(defn scale [c v]
  {:x (* c (:x v))
   :y (* c (:y v))})

; https://en.wikipedia.org/wiki/Elastic_collision
(defn collision-v1 [v-a v-b c-a c-b]
  (let [v-prime (subtract v-a
                          (scale (/ (dot (subtract v-a v-b)
                                         (subtract c-a c-b))
                                    (Math/pow (magnitude (subtract c-a c-b))
                                             2))
                                 (subtract c-a c-b)))]
    (if (or (Double/isNaN (:x v-prime)) (Double/isNaN  (:y v-prime)))
      v-a
      v-prime))
  )
(defn collision-v2 [v-a v-b c-a c-b] (collision-v1 v-b v-a c-b c-a))