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
    "regular" {:friction 0.995, :board :felt}
    "ice" {:friction 0.999, :board :ice}
    "rock" {:friction 0.98, :board :rock}
    {:friction 0.995, :board :felt}                             ; default
    )))
(defn create-ball [num] "Creates a basic ball for "
  {:num num :x 0 :y 0 :radius 10 :weight 100 :velocity {:x 0 :y 0} :spin {:x 0 :y 0 :z 0}})
(defn create-table [material-type size] {
            :balls (map create-ball (drop 1 (range 16)))
            :friction (:friction (material-type->properties material-type))
            :width (:width size)
            :height (:height size)
})

(defn create-testing-table [size]
  (let [b1 (-> (create-ball 1)
               (assoc-in [:velocity :x] 10)
           )
        b2 (-> (create-ball 2)
               (assoc-in [:x] 40)
               (assoc-in [:y] 5)
               )]
   {
    :balls    [b1 b2]
    :friction (:friction (material-type->properties "regular"))
    :width    (:width size)
    :height   (:height size)
  }))

(defn step-ball [step-size table ball]
  (let [friction (:friction table)
        x-velo (get-in ball [:velocity :x])
        y-velo (get-in ball [:velocity :y])
        step-x-velo (/ x-velo step-size)
        step-y-velo (/ y-velo step-size)
        ]
    (-> ball
        (assoc :x (+ (:x ball) (/ x-velo step-size)))
        (assoc :y (+ (:y ball) (/ y-velo step-size)))
        (assoc-in [:velocity :x] (- x-velo (* friction step-x-velo)))
        (assoc-in [:velocity :y] (- y-velo (* friction step-y-velo)))
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
(defn intersects? [ball-a ball-b]
  (let [x-diff (- (:x ball-a) (:x ball-b))
        y-diff (- (:y ball-a) (:y ball-b))
        dist (Math/sqrt (+ (* x-diff x-diff) (* y-diff y-diff)))]
    (< dist (+ (:radius ball-a) (:radius ball-b)))          ; If the balls are too close then they intersect.
  ))
(defn intersect-list [balls] "With 15 balls, just do a brute force comparison of all combinations."
  (loop [b (first balls)
         rem (rest balls)
         v []]
    (if (empty? rem)
      v
      (recur
        (first rem)
        (rest rem)
        (concat v (filter some? (map
                                (fn [a] (if (intersects? a b)
                                          [a b])) rem)))))))
(defn translate-frame-of-reference [v-a v-b]
  [(-> v-a
       (assoc :x (- (:x v-a) (:x v-b)))
       (assoc :y (- (:y v-a) (:y v-b))))
   (-> v-b
       (assoc :x 0)
       (assoc :y 0))])
(defn v-dot [v-a v-b]
  (+ (* (:x v-a) (:x v-b))
     (* (:y v-a) (:y v-b))))
(defn v-magnitude [v]
  (Math/sqrt (+ (Math/pow (:x v) 2)
                (Math/pow (:y v) 2))))
(defn v-sub [v-a v-b]
  {:x (- (:x v-a) (:x v-b))
   :y (- (:y v-a) (:y v-b))})
(defn v-scale [c v]
  {:x (* c (:x v))
   :y (* c (:y v))})

; https://en.wikipedia.org/wiki/Elastic_collision
(defn v1-prime [v-a v-b c-a c-b]
    (v-sub v-a
           (v-scale (/ (v-dot (v-sub v-a v-b)
                              (v-sub c-a c-b))
                 (Math/pow (v-magnitude (v-sub c-a c-b))
                           2))
              (v-sub c-a c-b)))
)
(defn v2-prime [v-a v-b c-a c-b] (v1-prime v-b v-a c-b c-a))

(comment
  (def b-a (-> (create-ball 1)
               (assoc-in [:velocity :x] 10)
               (assoc-in [:velocity :y] 0)))
  (def b-b (-> (create-ball 2)
               (assoc-in [:velocity :x] 0)
               (assoc-in [:velocity :y] 0)
               (assoc-in [:x] 7.23)                           ; 7.23 is roughly the center for a collision between two balls of radius 5
               (assoc-in [:y] 7.23)))
  (def v-a (:velocity b-a))
  (def v-b (:velocity b-b))
  (def c-a {:x (:x b-a) :y (:y b-a)})
  (def c-b {:x (:x b-b) :y (:y b-b)})
  (v1-prime v-a v-b c-a c-b)
  (v2-prime v-a v-b c-a c-b)
  (v1-prime (:velocity b-a)
            (:velocity b-b)
            {:x (:x b-a) :y (:y b-a)}
            {:x (:x b-b) :y (:y b-b)})
  (v-sub (:velocity b-a) (:velocity b-b))
  (v-dot (:velocity b-a) (:velocity b-b))
,)
; Physics for math equations
; Vai = Vbf
; Vaf = -Vbi
; Note: Since we're not striking the balls directly at the center every time, I think we'll need to adjust the forces based on angle towards center. Then apply the change in velocities based on impacted force towards the center.
; Will completely avoid spin for now.
(defn handle-collision [ball-a ball-b]
  (let [[v-a v-b] (translate-frame-of-reference (:velocity ball-a) (:velocity ball-b))]

  ))

(defn step-table [step-size table] "Basic idea for the algorithm is move balls, check intersections, update velocities as if they bounced. Let the updated velocities take care of fixing the positions over time."
  (let [balls (map (partial step-ball step-size table) (:balls table))]
    ; TODO collision handling here.
    (assoc table :balls balls)
  ))
(defn sim-table [max-steps step-size table]
  (loop [t table
         ct 0]
    (prn ct)
    (if (= ct max-steps)
      t
      (recur (step-table step-size t) (inc ct)))))
(defn sim-ball-alone [iBall max-steps step-size table]
  (let [ball iBall]
    (loop [b ball
           ct 0]
      (if (or (= ct max-steps) (stopped? b))
        (do (prn "stopped?") (stop-ball b))
        (recur (step-ball step-size table b) (inc ct))))))

(comment "Ball defs"
  (def default-table (create-table :regular 200))
  (def test-table (create-testing-table 200))
  (def stationary-ball (create-ball 1))
  (def x-moving-ball (assoc-in (create-ball 1) [:velocity :x] 10))
  (def y-moving-ball (assoc-in (create-ball 1) [:velocity :y] -10))
  (def xy-moving-ball (-> (create-ball 1)
                         (assoc-in [:velocity :x] 100)
                         (assoc-in [:velocity :y] -100)))
  (def ball xy-moving-ball)
  (def table default-table)
  (def table test-table)
  (def step-size 100)
  (def friction 0.97)
  (sim-ball-alone ball 2000 100 table)
  (step-ball ball 100 table ball)
  (step-table 10 table)
  ,
)