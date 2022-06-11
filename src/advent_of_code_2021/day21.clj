(ns advent-of-code-2021.day21)

(def starting-pos-test [4 8])
(def starting-pos-real [8 5])

(defn roll-deterministic [num-rolls n]
  (let [c (cycle (range 1 101))]
    [(apply + (take n (drop num-rolls c))) (+ num-rolls n)]))

(defn next-player [p]
  (if (= p :p1) :p2 :p1))

(defn new-pos [current-pos d]
  (mod (+ current-pos d) 10))

(defn step-player [player d]
  (let [pos (new-pos (:pos player) d)]
    {:pos pos :score (+ (:score player) (inc pos))}))

(defn step-players [turn p1 p2 d]
  (if (= :p1 turn)
    [(step-player p1 d) p2]
    [p1 (step-player p2 d)]))

(defn step [state]
  (let [[d num-rolls] (roll-deterministic (:num-rolls state) 3)
        [p1 p2] (step-players (:turn state) (:p1 state) (:p2 state) d)]
    {:num-rolls num-rolls
     :turn (next-player (:turn state))
     :p1 p1
     :p2 p2}))

(defn win? [state]
  (or
   (>= (get-in state [:p1 :score]) 1000)
   (>= (get-in state [:p2 :score]) 1000)))

(defn play [state]
  (loop [s state]
    (println s)
    (cond
      (win? s) s
      :else (recur (step s)))))

(defn final-score [state]
  (let [losing-p  (if (>= (get-in state [:p1 :score]) 1000) :p2 :p1)]
    (* (:num-rolls state) (get-in state [losing-p :score]))))

(defn task01 [[p1 p2]]
  (let [state {:num-rolls 0
               :turn :p1
               :p1 {:pos (dec p1) :score 0}
               :p2 {:pos (dec p2) :score 0}}
        final-state (play state)]
    (println state);
    (println final-state);
    (println (final-score final-state));
    ))

;; ==============================
;; Multiple universes below here

(defn initial-map [[p1 p2]]
  {{:turn :p1
    :p1 {:pos (dec p1) :score 0}
    :p2 {:pos (dec p2) :score 0}} (long 1)})

(defn step-single-player-state [state]
  (let [pos (:pos state)
        score (:score state)
        deltas [[3 1] [4 3] [5 6] [6 7] [7 6] [8 3] [9 1]]
        f (fn [[dpos count]]
            (let [p (mod (+ pos dpos) 10)]
              (vector {:pos p :score (+ score (inc p))} (long count))))]
    (vec (map f deltas))))

(defn step-map [[state num]]
  (let [current-p (:turn state)
        next-p (next-player current-p)
        current-p-state (current-p state)
        next-p-state (next-p state)
        stepped-states (step-single-player-state current-p-state)
        f (fn [[stepped-state n]]
            ;(println n "*" num)
            [{:turn next-p
              current-p stepped-state
              next-p next-p-state} (* (long n) (long num))])]
    (apply hash-map (mapcat f stepped-states))))

(defn win2? [state]
  (or
   (>= (get-in state [:p1 :score]) 21)
   (>= (get-in state [:p2 :score]) 21)))

(defn step-universe [universe-state]
  (loop [result []
         to-process universe-state]
    (cond
      (empty? to-process) (apply merge-with + result)
      :else
      (let [current (first to-process)
            stepped (if (win2? (first current)) {(first current) (last current)} (step-map current))]
        (recur (conj result stepped) (rest to-process))))))

(defn play-universe [universe-state]
  (loop [current universe-state]
    (cond
      (empty? (filter #(not (win2? (first %))) current)) current
      :else (recur (step-universe current)))))

(defn task02 [[p1 p2]]
  (let [state (initial-map [p1 p2])
        final (play-universe state)
        count-p1 (reduce + (map last (filter #(>= (get-in (first %) [:p1 :score]) 21) final)))
        count-p2 (reduce + (map last (filter #(>= (get-in (first %) [:p2 :score]) 21) final)))]
    (if (> count-p1 count-p2) (println "Player 1 wins more often:" count-p1) (println "Player 2 wins more often:" count-p2))))

(defn day21 []
  (time (task01 starting-pos-real));
  (time (task02 starting-pos-real));
  )

(defn -main [] (day21))