(ns clj-billiards.core
  (:require [clojure.core.async :as a
                                :refer [>! <! >!! <!! go chan buffer close! thread
                                        alts! alts!! timeout]]
            [clj-billiards.vector :as v]))

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
  {:num num :x 0 :y 0 :radius 10 :weight 100
   :velocity {:x 0 :y 0}})
(defn create-table [material-type width height]
  {
   :balls    (map create-ball (range 16))
   :friction (:friction (material-type->properties material-type))
   :width    width
   :height   height
   :vertical-stack false
  })

; cos 45 == .525321988818
; sin 45 == .850903524534
;            [0,0]
;         (-1,1)[1,1]
;      [-2,2]<0,2>(2,2)
;   (-3,3)[-1,3](1,3)[3,3]
; [-4,4](-2,4)(0,4)[2,4](4,4)
(defn racked-positions [radius vertical-stack offset]
  (let [r (* 1.05 radius)
        w (* r (Math/cos 45))
        h (* r (Math/sin 45))
        pos (if vertical-stack
              (fn [x y] {:x (+ (:x offset) (* x w)) :y (+ (:y offset) (* y h))})
              (fn [y x] {:x (+ (:x offset) (* x h)) :y (+ (:y offset) (* y w))}))]
    {
     :solids  (shuffle [(pos 0 0) (pos 1 1) (pos -2 2) (pos -1 3)
                        (pos 3 3) (pos -4 4) (pos 2 4)])
     :stripes (shuffle [(pos -1 1) (pos 1 2) (pos -3 3) (pos 1 3)
                        (pos -2 4) (pos 0 4) (pos 4 4)])
     :eight   [(pos 0 2)]
     :cue     [(pos 0 -10)]
     }))
(defn rack-balls [balls vertical-stack offset]
  (let [p (racked-positions (:radius (first balls)) vertical-stack offset)]
    (loop [[b & rem] balls
           p p
           racked-balls []]
      (if (nil? b)
        racked-balls
        (let [next-pos (cond
                         (= 0 (:num b)) :cue
                         (< 0 (:num b) 8) :solids
                         (= 8 (:num b)) :eight
                         (< 8 (:num b)) :stripes
                         :else :missing)]
          (if (= :missing next-pos)
            racked-balls
            (recur rem
                   (assoc p next-pos (rest (get p next-pos)))
                   (conj racked-balls (-> b
                                          (assoc :x (:x (first (get p next-pos))))
                                          (assoc :y (:y (first (get p next-pos)))))))))))))
(defn rack-table [table]
  (assoc table :balls (sort-by :num (rack-balls (:balls table)
                                                (:vertical-stack table)
                                                {:x (/ (:width table) 2) :y (/ (:height table) 2)}))))

(defn step-ball [step-size table ball]
  (let [friction (:friction table)
        x-velo (get-in ball [:velocity :x])
        y-velo (get-in ball [:velocity :y])
        step-x-velo (- x-velo (* friction (/ x-velo step-size)))
        step-y-velo (- y-velo (* friction (/ y-velo step-size)))
        ]
    (-> ball
        (assoc :x (+ (:x ball) (/ x-velo step-size)))
        (assoc :y (+ (:y ball) (/ y-velo step-size)))
        (assoc-in [:velocity :x] (if (< 0.00001 step-x-velo) step-x-velo 0))
        (assoc-in [:velocity :y] (if (< 0.00001 step-y-velo) step-y-velo 0))
    )))
(defn stopped? [ball]
  (let [velocity (:velocity ball)]
    (and (< (:x velocity) 0.01) (< (:y velocity) 0.01))))
(defn stop-ball [ball]
  (-> ball
      (assoc-in [:velocity :x] 0)
      (assoc-in [:velocity :y] 0)
))

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

; Will completely avoid spin for now.
(defn handle-collision [ball-a ball-b]
  (if (intersects? ball-a ball-b)
    (let [ca {:x (:x ball-a) :y (:y ball-a)}
          cb {:x (:x ball-b) :y (:y ball-b)}
          va-prime (v/collision-v1 (:velocity ball-a) (:velocity ball-b) ca cb)
          vb-prime (v/collision-v2 (:velocity ball-a) (:velocity ball-b) ca cb)]
      ;(prn "Collision between " (:num ball-a) " and " (:num ball-b))
      ;(prn ca cb)
      ;(prn (:velocity ball-a) (:velocity ball-b) (:num ball-a))
      ;(prn va-prime vb-prime)
      {
       :ball-a (-> ball-a
                   (assoc-in [:velocity :x] (:x va-prime))
                   (assoc-in [:velocity :y] (:y va-prime)))
       :ball-b (-> ball-b
                   (assoc-in [:velocity :x] (:x vb-prime))
                   (assoc-in [:velocity :y] (:y vb-prime)))
       })
    nil))
(defn handle-table-collision [balls table]
  (reduce
      (fn [l ball] (conj l (cond
                             (> 5 (:x ball)) (assoc-in ball [:velocity :x] (Math/abs ^double (get-in ball [:velocity :x])))
                             (< (- (:width table) 5) (:x ball)) (assoc-in ball [:velocity :x] (* -1 (Math/abs ^double (get-in ball [:velocity :x]))))
                             (> 5 (:y ball)) (assoc-in ball [:velocity :y] (Math/abs ^double (get-in ball [:velocity :y])))
                             (< (- (:height table) 5) (:y ball)) (assoc-in ball [:velocity :y] (* -1 (Math/abs ^double (get-in ball [:velocity :y]))))
                             :else ball
                             )))
      [] balls))

(defn step-table [step-size table] "Basic idea for the algorithm is move balls, check intersections, update velocities as if they bounced. Let the updated velocities take care of fixing the positions over time."
  (let [balls (map (partial step-ball step-size table) (:balls table))
        collision-balls (filter some?
                                (map (fn [[ball-a ball-b]] (handle-collision ball-a ball-b))
                                     (intersect-list balls)))
        end-step-balls (vals (reduce #(-> %1
                                          (assoc (get-in %2 [:ball-a :num]) (:ball-a %2))
                                          (assoc (get-in %2 [:ball-b :num]) (:ball-b %2)))
                                     (reduce #(assoc %1 (:num %2) %2) {} balls)
                                     collision-balls))
        table-collision-balls (handle-table-collision end-step-balls table)]
    (assoc table :balls (sort-by :num table-collision-balls))))
(defn sim-table [max-steps step-size table]
  (loop [t table
         ct 0]
    (prn ct)
    (if (= ct max-steps)
      t
      (recur (step-table step-size t) (inc ct)))))

(comment "Ball defs"
  (def table (rack-table (create-table :regular 400 300)))
  (def ball (-> (create-ball 1)
                (assoc-in [:velocity :x] 100)
                (assoc-in [:velocity :y] -100)))
  (def balls (:balls table))
  (def all-balls (:balls table))
  (def step-size 100)
  (def friction 0.97)
  (def friction 0.999)
  (step-ball ball 100 table ball)
  (step-table 10 table)
  (sim-table 2000 100 table)
  ,
)