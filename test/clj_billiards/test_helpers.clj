(ns clj-billiards.test-helpers)

(defn rel= [a b epsilon]
  (<= (abs (- a b)) epsilon))
(defn v-rel= [a b epsilon]
  (and
    (rel= (:x a) (:x b) epsilon)
    (rel= (:y a) (:y b) epsilon)))