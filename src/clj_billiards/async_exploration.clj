(ns clj-billiards.async-exploration
  (:require [clojure.core.async :refer :all]))

(defn sim [max-steps k]
  (loop [i k
         ct 0]
    (prn ct)
    (if (= ct max-steps)
      i
      (recur (+ i 2) (inc ct)))))

(sim 200000 2)

(def c-in (chan))
(def c-out (chan))
(def i 0)

(defn c-sim [max-steps k in-chan out-chan]
  (go (loop [i k
             ct 0]
        (let [input (poll! in-chan)]
          (if (= 0 (mod ct 100)) (prn ct))
          (if (some? input) (prn (str "***********" input)))
          (if (= ct max-steps)
            (>! out-chan i)
            (recur (+ i 2) (inc ct)))))))

(c-sim 100000 0 c-in c-out)
(go (>! c-in "**********IN**********"))

(go (prn (str "C-OUT " (<! c-out))))