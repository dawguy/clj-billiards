(ns clj-billiards.core
  (:require [clojure.core.async :as a
                                :refer [>! <! >!! <!! go chan buffer close! thread
                                        alts! alts!! timeout]])
  )

; Idea here. Create a billiards game based on Tables that players can start playing at.
; Then simulate a pool hall tournament by having a bracket and as each match ends new players are able to get assigned to the table.

(def tables (atom {}))
(defprotocol Table "A representation of a billiards table. Important attributes: player-a, player-b, balls, ."
  (start [this score-channel input-channel])
  (stop [this])
  (step [this])
  (add-player [player])
  (shot [shot])
)
(defrecord PoolTable [id player-a player-b
                      balls score-channel input-channel]
  Table
  (start [this score-channel input-channel] (let [table (-> this
                                              (assoc :score-channel score-channel)
                                              (assoc :running true))]
                                (swap! tables assoc (:id table) table)))

  )
(def game-chan (chan))
(defn material-type->properties
  ([] (material-type->properties "regular"))
  ([material-type]
  (case material-type
    "regular" {:friction 0.95, :board :felt}
    "ice" {:friction 0.98, :board :ice}
    "rock" {:friction 0.9, :board :rock}
    {:friction 0.95, :board :felt}                             ; default
    )))
(defn create-ball [num] "Creates a basic ball for "
  {:num num :x 0 :y 0 :radius 10 :weight 100 :velocity {:x 0 :y 0} :spin {:x 0 :y 0 :z 0}})
(defn create-table [material-type size] {
            :balls (map create-ball (drop 1 (range 15)))
            :friction (:friction (material-type->properties material-type))
            :width (:width size)
            :height (:height size)
})

(defn step-ball [step-size ball table]
  (let [friction (:friction table)
        x-velo (* friction (get-in ball [:velocity :x]))
        y-velo (* friction (get-in ball [:velocity :y]))
        ]
    (-> ball
        (assoc :x (+ (:x ball) (/ (* x-velo) step-size)))
        (assoc :y (+ (:y ball) (/ (* y-velo) step-size)))
        (assoc-in [:velocity :x] x-velo)
        (assoc-in [:velocity :y] y-velo)
    )))
(defn stopped? [ball]
  (let [velocity (:velocity ball)]
    (and (< (:x velocity) 0.01) (< (:y velocity) 0.01))))
(defn stop-ball [ball]
  (-> ball
      (assoc-in [:velocity :x] 0)
      (assoc-in [:velocity :y] 0)
      (assoc-in [:spin :x] 0)
      (assoc-in [:spin :y] 0)
      (assoc-in [:spin :z] 0)))
(defn sim [max-steps step-size table]
  (let [ball (first (:balls table))]
    (loop [b ball
           ct 0]
      (if (or (= ct max-steps) (stopped? b))
        (do (prn "stopped?") (stop-ball b))
        (recur (step-ball step-size b table) (inc ct))))))

(comment "Ball defs"
  (def default-table (create-table :regular 200))
  (def stationary-ball (create-ball 1))
  (def x-moving-ball (assoc-in (create-ball 1) [:velocity :x] 10))
  (def y-moving-ball (assoc-in (create-ball 1) [:velocity :y] -10))
  (def xy-moving-ball (-> (create-ball 1)
                         (assoc-in [:velocity :x] 10)
                         (assoc-in [:velocity :y] -10)))
  (def ball x-moving-ball)
  (def table default-table)
  (def step-size 100)
  (def friction 0.97)
  (step-ball 100 ball table)
  ,
)