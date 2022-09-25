(ns fuze.core
  (:gen-class))

(defn game-over? [game-state]
  (if (every? identity
              (map (fn [activation-block]
                     (some identity
                           (map (fn [active-block]
                                  (if (= activation-block active-block)
                                    true
                                    false))
                                (:active-blocks game-state))))
                   (:activation-blocks game-state)))
    (assoc game-state :game-over true :end-state "You win!")
    game-state))

(defn update-active-blocks [game-state]
  (let [new-active-blocks
        (filter not-empty
                (map
                 (fn [inactive-block]
                   (apply
                    conj (filter some?
                                 (map
                                  (fn [active-block]
                                    (if
                                     (or
                                      (and
                                       (= (first inactive-block) (first active-block))
                                       (or (= (second inactive-block) (inc (second  active-block)))
                                           (= (second inactive-block)  (inc (second active-block)))))
                                      (and
                                       (= (second  inactive-block) (second active-block))
                                       (or (= (first inactive-block) (inc (first  active-block)))
                                           (= (first inactive-block)  (dec (first active-block))))))
                                      inactive-block
                                      nil)) (:active-blocks game-state)))))
                 (:inactive-blocks game-state)))
        new-inactive-blocks (mapcat
                             (fn [[x n]] (repeat n x))
                             (apply merge-with - (map frequencies [(:inactive-blocks game-state) new-active-blocks])))]
    (assoc game-state
           :active-blocks (apply conj (:active-blocks game-state) new-active-blocks)
           :inactive-blocks new-inactive-blocks)))

(defn update-position [input game-state game-config]
  (if (nil? ((keyword input) (:key-bindings game-config)))
    game-state
    (let [move ((keyword input) (:key-bindings game-config))
          new-active-blocks (into [] (map #(into [] (map + move %)) (:active-blocks game-state)))]
      (if (empty?
           (filter #(or (< (first %) 0)
                        (< (second %) 0)
                        (> (first %) (dec (:breadth game-config)))
                        (> (second %) (dec (:length game-config)))) ;Check if it is out of bounds
                   new-active-blocks))
        (assoc game-state :active-blocks new-active-blocks)
        game-state))))

(defn draw-board
  "Prints the board(non-lazy)"
  [game-config game-state]
  (do
    (print (str (char 27) "[2J")) ; clear screen
    (print (str (char 27) "[;H")) ; move cursor to the top left corner of the screen
    (doall
     (for [x (range (:breadth game-config))
           y (range (:length game-config))]
       (cond
         (some #(= % [x y]) (:active-blocks game-state))  (if  (= y (dec (:length game-config)))
                                                            (println "[  *  ]")
                                                            (print "[  *  ]"))
         (some #(= % [x y]) (:inactive-blocks game-state))  (if  (= y (dec (:length game-config)))
                                                              (println "[  @  ]")
                                                              (print "[  @  ]"))
         (some #(= % [x y]) (:activation-blocks game-state)) (if  (= y (dec (:length game-config)))
                                                               (println "[  X  ]")
                                                               (print "[  X  ]"))
         (some #(= % [x y]) (:hyperactive-blocks game-state)) (if  (= y (dec (:length game-config)))
                                                                (println "[  #  ]")
                                                                (print "[  #  ]"))
         :else (if (= y (dec (:length game-config)))
                 (println "[     ]")
                 (print "[     ]")))))))

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
        (let [new-game-state (game-over?
                              (update-active-blocks
                               (update-position input game-state game-config)))]
          (draw-board game-config new-game-state)
          (recur (read-line) new-game-state))))))

