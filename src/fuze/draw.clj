(ns fuze.draw)


(defn draw-board
  "Prints the board(non-lazy)"
  [game-config game-state]
  (do (print (str (char 27) "[2J")) ; clear screen
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

