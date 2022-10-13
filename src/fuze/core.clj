(ns fuze.core
  (:gen-class)
  (:require [fuze.update :as update])
  (:require [fuze.draw :as draw]))

(defn -main []
  (let [game-config {:length 9
                     :breadth 6
                     :key-bindings {:w [-1 0]
                                    :a [0 -1]
                                    :s [1 0]
                                    :d [0 1]}}]
    (loop [input ""
           game-state {:active-blocks [[0 0] [2 5]]
                       :inactive-blocks [[0 2] [1 4] [2 7] [5 6] [5 5]]
                       :activation-blocks [[2 2] [2 4] [5  7]  [5  8]]
                       :game-over false
                       :end-state "You lost"}]
      (if (or (= input "q") (:game-over game-state))
        (println  "Thanks for playing! " (:end-state game-state))
        (let [new-game-state (update/game-over?
                             (update/update-blocks
                             (update/update-position input game-state game-config)))]
          (draw/draw-board game-config new-game-state)
          (recur (read-line) new-game-state))))))

