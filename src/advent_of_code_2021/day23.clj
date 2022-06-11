(ns advent-of-code-2021.day23
  (:require [clojure.set]
            [clojure.pprint])
  (:gen-class))

; For testing purposes
(def sample-graph {:hall [\. \. \. \. \.]
                   \A [\B \B]
                   \B [\A \A]})

; For testing purposes. This is how the move table ideally looks like for the sample graph
; Note that moves directly from one room to another are not strictly necessary but decrease the size of the search graph
(def move-table {[[:hall 0] [\A 0]] {:cost 3 :way [[:hall 1]]}
                 [[:hall 0] [\A 1]] {:cost 4 :way [[:hall 1] [\A 0]]}
                 [[:hall 0] [\B 0]] {:cost 5 :way [[:hall 1] [:hall 2]]}
                 [[:hall 0] [\B 1]] {:cost 6 :way [[:hall 1] [:hall 2] [\B 0]]}

                 [[:hall 1] [\A 0]] {:cost 2 :way []}
                 [[:hall 1] [\A 1]] {:cost 3 :way [[\A 0]]}
                 [[:hall 1] [\B 0]] {:cost 4 :way [[:hall 2]]}
                 [[:hall 1] [\B 1]] {:cost 5 :way [[:hall 2] [\B 0]]}

                 [[:hall 2] [\A 0]] {:cost 2 :way []}
                 [[:hall 2] [\A 1]] {:cost 3 :way [[\A 0]]}
                 [[:hall 2] [\B 0]] {:cost 2 :way []}
                 [[:hall 2] [\B 1]] {:cost 3 :way [[\B 0]]}

                 [[:hall 3] [\A 0]] {:cost 4 :way [[:hall 2]]}
                 [[:hall 3] [\A 1]] {:cost 5 :way [[:hall 2] [\A 0]]}
                 [[:hall 3] [\B 0]] {:cost 2 :way []}
                 [[:hall 3] [\B 1]] {:cost 3 :way [[\B 0]]}

                 [[:hall 4] [\A 0]] {:cost 5 :way [[:hall 3] [:hall 2]]}
                 [[:hall 4] [\A 1]] {:cost 6 :way [[:hall 3] [:hall 2] [\A 0]]}
                 [[:hall 4] [\B 0]] {:cost 3 :way [[:hall 3]]}
                 [[:hall 4] [\B 1]] {:cost 4 :way [[:hall 3] [\B 0]]}

                 [[\A 0] [:hall 0]] {:cost 3 :way [[:hall 1]]}
                 [[\A 0] [:hall 1]] {:cost 2 :way []}
                 [[\A 0] [:hall 2]] {:cost 2 :way []}
                 [[\A 0] [:hall 3]] {:cost 4 :way [[:hall 2]]}
                 [[\A 0] [:hall 4]] {:cost 5 :way [[:hall 3] [:hall 2]]}

                 [[\A 0] [\B 0]] {:cost 4 :way [[:hall 2]]}
                 [[\A 0] [\B 1]] {:cost 5 :way [[:hall 2] [\B 0]]}

                 [[\A 1] [:hall 0]] {:cost 4 :way [[:hall 1] [\A 0]]}
                 [[\A 1] [:hall 1]] {:cost 3 :way [[\A 0]]}
                 [[\A 1] [:hall 2]] {:cost 3 :way [[\A 0]]}
                 [[\A 1] [:hall 3]] {:cost 5 :way [[:hall 2] [\A 0]]}
                 [[\A 1] [:hall 4]] {:cost 6 :way [[:hall 3] [:hall 2] [\A 0]]}

                 [[\A 1] [\B 0]] {:cost 5 :way [[\A 0] [:hall 2]]}
                 [[\A 1] [\B 1]] {:cost 6 :way [[\A 0] [:hall 2] [\B 0]]}

                 [[\B 0] [:hall 0]] {:cost 5 :way [[:hall 1] [:hall 2]]}
                 [[\B 0] [:hall 4]] {:cost 3 :way [[:hall 3]]}
                 [[\B 0] [:hall 1]] {:cost 4 :way [[:hall 2]]}
                 [[\B 0] [:hall 2]] {:cost 2 :way []}
                 [[\B 0] [:hall 3]] {:cost 2 :way []}

                 [[\B 0] [\A 0]] {:cost 4 :way [[:hall 2]]}
                 [[\B 0] [\A 1]] {:cost 5 :way [[:hall 2] [\A 0]]}

                 [[\B 1] [:hall 0]] {:cost 6 :way [[:hall 1] [:hall 2] [\B 0]]}
                 [[\B 1] [:hall 1]] {:cost 5 :way [[:hall 2] [\B 0]]}
                 [[\B 1] [:hall 2]] {:cost 3 :way [[\B 0]]}
                 [[\B 1] [:hall 3]] {:cost 3 :way [[\B 0]]}
                 [[\B 1] [:hall 4]] {:cost 4 :way [[:hall 3] [\B 0]]}

                 [[\B 1] [\A 0]] {:cost 5 :way [[\B 0] [:hall 2]]}
                 [[\B 1] [\A 1]] {:cost 6 :way [[\B 0] [:hall 2] [\A 0]]}})

; Cost table of how much steps it takes to move from one hall position to the entrance of a room
; This can definitely be calculated in some way but I'm just really tired of this task
(def cost-table {[0 \A] 2
                 [0 \B] 4
                 [0 \C] 6
                 [0 \D] 8

                 [1 \A] 1
                 [1 \B] 3
                 [1 \C] 5
                 [1 \D] 7

                 [2 \A] 1
                 [2 \B] 1
                 [2 \C] 3
                 [2 \D] 5

                 [3 \A] 3
                 [3 \B] 1
                 [3 \C] 1
                 [3 \D] 3

                 [4 \A] 5
                 [4 \B] 3
                 [4 \C] 1
                 [4 \D] 3

                 [5 \A] 7
                 [5 \B] 5
                 [5 \C] 3
                 [5 \D] 1

                 [6 \A] 8
                 [6 \B] 6
                 [6 \C] 4
                 [6 \D] 2})

(defn print-graph [{hall :hall :as rest}]
  (let [rooms (dissoc rest :hall)
        length-hall (+ (count hall) (count rooms))
        length-line (+ length-hall 2)
        printable-hall  (into []
                              (concat
                               (take 1 hall)
                               (interleave (drop 1 hall) (repeat (count rooms) \.))
                               (take-last 2 hall)))
        transposed-rooms (apply map vector (vals rooms))
        depth (count (first rooms))]
    (println (apply str (repeat length-line \#)))
    (print \#)
    (print (apply str printable-hall))
    (println \#)
    (print "##")
    (print (apply str (interleave (repeat (count rooms) \#) (first transposed-rooms))))
    (println "###")
    (loop [i 1]
      (when (< i depth)
        (print "  ")
        (print (apply str (interleave (repeat (count rooms) \#) (nth transposed-rooms i))))
        (println \#)
        (recur (inc i))))
    (print "  ")
    (println (apply str (repeat (inc (* 2 (count rooms))) \#)))))

(defn build-graph [rooms]
  (let [length-hallway (+ 4 (dec (count rooms)))
        hallway (vec (repeat length-hallway \.))
        graph (assoc rooms :hall hallway)]
    graph))

(defn room-entrance-indices [rooms]
  (loop [entrance-map {}
         current (first (keys rooms))
         remaining (rest (keys rooms))]
    (if (nil? current)
      entrance-map
      (let [idx (- (int current) (int \A))]
        (recur (assoc entrance-map current [(inc idx) (inc (inc idx))]) (first remaining) (rest remaining))))))

(defn move-hall-to-room [hall-idx room room-idx entrance-left-idx entrance-right-idx]
  (let [[cost places] (cond
                        (some #(= hall-idx %) [entrance-left-idx entrance-right-idx]) [1 []]
                        (< hall-idx entrance-left-idx) [(get cost-table [hall-idx room])
                                                        (vec (map #(vector :hall %) (range (inc hall-idx) (inc entrance-left-idx))))]
                        (> hall-idx entrance-right-idx) [(get cost-table [hall-idx room])
                                                         (vec (map #(vector :hall %) (range entrance-right-idx hall-idx)))])]
    {:cost (+ cost (inc room-idx))
     :way   (apply conj places (map #(vector room %) (range 0 room-idx)))}))



(defn build-move-table-hall-to-room [hall-length room depth entrance-left-idx entrance-right-idx]
  (into {} (for [hall-idx (range hall-length)
                 room-idx (range depth)]
             (let [move (move-hall-to-room hall-idx room room-idx entrance-left-idx entrance-right-idx)]
               [[[:hall hall-idx] [room room-idx]] move]))))

(defn build-move-table
  "Builds a table of moves for the graph that looks like the sample move table.
   It currently only contains moves between a room and the hall. This does not impair the solvability of the task
   but makes the search take longer than necessary."
  [rooms]
  (let [graph (build-graph rooms)
        depth (count (first (vals rooms)))
        entrances (room-entrance-indices rooms)
        hall (get graph :hall)
        moves-from-hall
        (apply merge (for [room (keys rooms)]
                       (let [table (build-move-table-hall-to-room
                                    (count hall) room depth
                                    (first (get entrances room))
                                    (second (get entrances room)))]
                         table)))
        moves-to-hall (into {} (map (fn [[k v]] [[(second k) (first k)] v]) moves-from-hall))]
    (merge moves-from-hall moves-to-hall)))

(defn content  [[place idx] graph]
  (nth (get graph place) idx))

(defn room-finished? [room graph]
  (every? #(= room %) (get graph room)))

(defn room-almost-finished? [room graph]
  (and
   (not (room-finished? room graph))
   (every? #(or (= room %) (= \. %)) (get graph room))))

(defn finished? [graph]
  (and
   (every? (fn [[k _]] (room-finished? k graph)) (dissoc graph :hall))
   (every? #(= \. %) (get graph :hall))))

(defn positions [v]
  (vec (filter #(not (= \. (second %))) (map-indexed vector v))))

(defn all-positions [graph]
  (apply concat (for [[k v] graph]
                  (vec (for [p (positions v)]
                         (vec (flatten [k p])))))))

(defn unfinished-positions [graph]
  (let [finished-rooms (map first (filter (fn [[k _]] (room-finished? k graph)) (dissoc graph :hall)))
        almost-finished-rooms (map first (filter (fn [[k _]] (room-almost-finished? k graph)) (dissoc graph :hall)))
        positions-in-unfinished (filter (fn [[place _ _]] (not (some #(= % place) finished-rooms))) (all-positions graph))]
    (filter (fn [[place _ _]] (not (some #(= % place) almost-finished-rooms))) positions-in-unfinished)))


(defn occupied? [[place idx] graph]
  (not (= (content [place idx] graph) \.)))

(defn cost-modifier [c]
  (let [id (- (int c) (int \A))]
    (reduce * (repeat id 10))))

(defn possible-moves-for [[place idx c] graph move-table]
  (let [candidates
        (into {} (filter (fn [[[[p1 i1] [p2 i2]]  {way :way}]]
                           (and
                            (= [p1 i1] [place idx]) ; correct start index
                            (or (and
                                 (= p2 c) ; move into a room
                                 (room-almost-finished? p2 graph)) ; that can be moved into
                                (= p2 :hall)) ; move into a hall
                            (not (occupied? [p2 i2] graph)) ; target not occupied
                            (or (empty? way) (not-any? #(occupied? % graph) way)))) ; way not occupied
                         move-table))
        into-rooms (into {} (filter (fn [[[[_ _] [p2 _]] _]] (not (= :hall p2))) candidates))
        moves (if (empty? into-rooms)
                candidates
                (let [temp (apply max-key (fn [[[[_ _] [_ i2]] _]] i2) into-rooms)]
                  {(first temp) (last temp)}))]
    (map (fn [[k v]]
           [k {:cost (* (get v :cost) (cost-modifier c)) :way (get v :way)}]) moves)))

(defn possible-moves [graph move-table]
  (let [pos (unfinished-positions graph)
        candidates (vec (filter not-empty (apply concat (map #(possible-moves-for % graph move-table) pos))))
        into-rooms (into {} (filter (fn [[[[_ _] [p2 _]] _]] (not (= :hall p2))) candidates))
        moves (if (empty? into-rooms) candidates into-rooms)]
    (vec (sort-by #(:cost (last %)) moves))))

(defn make-move [graph [[[p1 i1] [p2 i2]] _]]
  (let [c (content [p1 i1] graph)
        list-p1 (assoc (get graph p1) i1 \.)
        list-p2 (assoc (get graph p2) i2 c)]
    (assoc (assoc graph p1 list-p1) p2 list-p2)))

(defn print-solution [{:keys [intermediate icosts]}]
  (loop [i 0]
    (when (< i (count intermediate))
      (print-graph (nth intermediate i))
      (println "Costs up to here:" (nth icosts i))
      (println "\n")
      (recur (inc i)))))

(defn solve-impl
  "Recursive search for "
  [{:keys [graph costs intermediate icosts] :as all} move-table current-best]
  (if (finished? graph)
    (do
      (println "Found Solution with costs:" costs)
      (print-solution all)
      all)
    (let [moves (possible-moves graph move-table)]
      (loop [current-move (first moves)
             remaining-moves (rest moves)
             in-loop-current-best current-best]
        (if (nil? current-move)
          in-loop-current-best
          (let [changed-graph (make-move graph current-move)
                changed-costs (+ costs (get (last current-move) :cost))
                step {:graph changed-graph
                      :costs  changed-costs
                      :intermediate (conj intermediate changed-graph)
                      :icosts (conj icosts changed-costs)}]
            (if (or (nil? in-loop-current-best) (< changed-costs (get in-loop-current-best :costs)))
              (let [temp (solve-impl step move-table in-loop-current-best)]
                (recur (first remaining-moves) (rest remaining-moves)
                       (cond
                         (and (nil? temp) (nil? in-loop-current-best)) nil
                         (and (nil? temp) (not (nil? in-loop-current-best))) in-loop-current-best
                         (and (not (nil? temp)) (nil? in-loop-current-best)) temp
                         :else (if (< (get temp :costs) (get in-loop-current-best :costs))
                                 temp
                                 in-loop-current-best))))
              (recur (first remaining-moves) (rest remaining-moves) in-loop-current-best))))))))

(defn solve [rooms]
  (let [graph (build-graph rooms)
        move-table (build-move-table rooms)
        temp {:graph graph :costs 0 :intermediate [graph] :icosts [0] :solved (finished? graph)}
        out (solve-impl temp move-table nil)]
    (println out)))


(defn task1 []
  (let [rooms {\A [\D \B]
               \B [\D \A]
               \C [\C \B]
               \D [\C \A]}]
    (solve rooms)))

(defn task2 []
  (let [rooms {\A [\D \D \D \B]
               \B [\D \C \B \A]
               \C [\C \B \A \B]
               \D [\C \A \C \A]}]
    (solve rooms)))

; Takes really long to execute... again...
(defn day23 []
  (task1)
  (task2))

(defn -main [] (day23))
