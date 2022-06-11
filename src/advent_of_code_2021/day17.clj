(ns advent-of-code-2021.day17
  (:gen-class))

(defn step [probe]
  {:x (+ (:x probe) (:vx probe))
   :y (+ (:y probe) (:vy probe))
   :vx (cond
         (< (:vx probe) 0) (inc (:vx probe))
         (> (:vx probe) 0) (dec (:vx probe))
         :else 0)
   :vy (dec (:vy probe))})

(defn inside? [probe target]
  (and
   (and
    (>= (:x probe) (first (:x target)))
    (<= (:x probe) (last (:x target))))
   (and
    (>= (:y probe) (first (:y target)))
    (<= (:y probe) (last (:y target))))))

(defn continue? [probe target]
  (cond
    (inside? probe target) false ; hit
    (and (> (:x probe) (last (:x target))) (> (:vx probe) 0)) false ; too far right 
    (and (< (:y probe) (first (:y target))) (< (:vy probe) 0)) false ; too low
    (and (= (:vx probe) 0) (not (and
                                 (>= (:x probe) (first (:x target)))
                                 (<= (:x probe) (last (:x target)))))) false ; wont hit
    :else true))

(defn fire [vx vy]
  {:x 0
   :y 0
   :vx vx
   :vy vy})

(defn hits? [vx vy target]
  (loop [probes [(fire vx vy)]]
    (let [current (last probes)]
      (cond
        (inside? current target) {:probes probes :hit true}
        (not (continue? current target)) {:probes probes :hit false}
        :else (recur (conj probes (step current)))))))

(defn max-y [probes]
  (apply max (map :y probes)))

(defn find-max-y [max-dx max-dy target]
  (loop [current-max 0
         dx 0
         dy 0]
    (cond
      (>= dy max-dy) current-max
      (>= dx max-dx) (recur current-max 0 (inc dy))
      :else (let [trajectory (hits? dx dy target)
                  m (max-y (:probes trajectory))]
              (if (:hit trajectory)
                (recur (max m current-max) (inc dx) dy)
                (recur current-max (inc dx) dy))))))

(defn count-hitting [max-dx min-dy max-dy target]
  (loop [count 0
         dx 0
         dy min-dy]
    (cond
      (>= dy max-dy) count
      (>= dx max-dx) (recur count 0 (inc dy))
      :else (let [trajectory (hits? dx dy target)]
              (recur (if (:hit trajectory) (inc count) count) (inc dx) dy)))))

(defn task01 [target]
  (println (find-max-y 200 1000 target)))

(defn task02 [target]
  (println (count-hitting 200 -500 1000 target)))

; target area: x=143..177, y=-106..-71
(defn day17 []
  (let [target {:y [-106 -71]
                :x [143 177]}]
    (time (task01 target));
    (time (task02 target));
    ))

(defn -main [] (day17))