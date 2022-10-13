(ns fuze.update)

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

(defn isAdj? [active-block inactive-block]
  (or
   (and
    (= (first inactive-block) (first active-block))
    (or (= (second inactive-block) (inc (second  active-block)))
        (= (second inactive-block)  (inc (second active-block)))))
   (and
    (= (second  inactive-block) (second active-block))
    (or (= (first inactive-block) (inc (first  active-block)))
        (= (first inactive-block)  (dec (first active-block)))))))


(defn update-active-blocks [game-state]
 (filter not-empty
         (map
          (fn [inactive-block]
            (apply
             conj (filter some?
                          (map
                           (fn [active-block]
                             (if (isAdj? active-block inactive-block)
                               inactive-block
                               nil)) (:active-blocks game-state)))))
          (:inactive-blocks game-state))))


(defn update-blocks [game-state]
  (let [new-active-blocks
        (update-active-blocks game-state)
        ; This is not ideal
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

