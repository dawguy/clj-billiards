(ns clj-billiards.test-helpers
  (:require [clojure.test :refer :all]))

(defn assert-rel-predicate
  "Base function copied from assert-predicate. Takes the expression in format
  (is (rel= (result-fn) expected epsilon). Then returns the result of result-fn in :actual and expected for :expected"
  [msg form]
  (let [args (rest form)
        pred (first form)
        exp (second args)]
    `(let [values# (list ~@args)
           calc-result# (first values#)
           bool-result# (apply ~pred values#)]
       (if bool-result#
         (do-report {:type     :pass, :message ~msg,
                     :expected '~exp, :actual calc-result#})
         (do-report {:type     :fail, :message ~msg,
                     :expected '~exp, :actual calc-result#}))
       bool-result#)))

(defmethod assert-expr 'rel= [msg form]
  (if (and (sequential? form) (function? (first form)))
    (assert-rel-predicate msg form)
    (assert-any msg form)))
(defmethod assert-expr 'v-rel= [msg form]
  (if (and (sequential? form) (function? (first form)))
    (assert-rel-predicate msg form)
    (assert-any msg form)))

(defn rel= [a b epsilon]
  (<= (abs (- a b)) epsilon))
(defn v-rel= [a b epsilon]
  (and
    (rel= (:x a) (:x b) epsilon)
    (rel= (:y a) (:y b) epsilon)))