(ns clj-billiards.core)

; Idea here. Create a billiards game based on Tables that players can start playing at.
; Then simulate a pool hall tournament by having a bracket and as each match ends new players are able to get assigned to the table.

(defprotocol Table "A representation of a billiards table"
  (start [this player-a player-b])
  (stop [this])
  (step [this])
)
(defprotocol NamedObj "An object that can say its name"
  (say-my-name [this]))
(defrecord PoolTable [id name]
  Table
  (start [this player-a player-b] (do (prn player-a) (prn player-b) (prn this)))
  NamedObj
  (say-my-name [this] (prn (:name this)))
  )

(def tables (atom []))
(swap! tables conj
      (map->PoolTable {:id "Hello" :name "David"}))

(say-my-name (first @tables))
(start (first @tables) "A" "B")