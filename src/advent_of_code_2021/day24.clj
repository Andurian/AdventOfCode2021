(ns advent-of-code-2021.day24
  (:require [advent-of-code-2021.util :as util]
            [clojure.string :as str]
            [seesaw.core])
  (:gen-class))

(def ^:private initial-state {\w 0
                              \x 0
                              \y 0
                              \z 0})

(defn- inp [state a stream]
  [(assoc state (first a) (first stream)) (rest stream)])

(defn- binary-op [op state a b]
  (let [val-a (get state (first a))
        val-b (if (some #(= (first b) %) (keys initial-state))
                (get state (first b))
                (Integer/parseInt b))
        res (op val-a val-b)]
    (assoc state (first a) res)))

(defn- add [state a b]
  (binary-op + state a b))

(defn- mul [state a b]
  (binary-op * state a b))

(defn- div [state a b]
  (binary-op quot state a b))

(defn- my-mod [state a b]
  (binary-op mod state a b))

(defn- eql [state a b]
  (binary-op (fn [v1 v2] (if (= v1 v2) 1 0)) state a b))

(defn- exec-line [line state stream]
  (let [[instruction op1 op2] (str/split line #" ")]
    (case instruction
      "inp" (inp state op1 stream)
      "add" [(add state op1 op2) stream]
      "mul" [(mul state op1 op2) stream]
      "div" [(div state op1 op2)  stream]
      "mod" [(my-mod state op1 op2) stream]
      "eql" [(eql state op1 op2) stream])))

(defn- run-prog [lines initial-stream]
  (loop [state initial-state
         intermediate [(clojure.string/join #" " ["init" "\t" (str state)])]
         stream initial-stream
         current-line (first lines)
         remaining-lines (rest lines)]
    (if (nil? current-line)
      [state (conj intermediate state)]
      (let [[new-state new-stream] (exec-line current-line state stream)
            new-intermediate (conj intermediate (clojure.string/join #" " [current-line "\t" (str new-state)]))]
        (recur new-state new-intermediate new-stream (first remaining-lines) (rest remaining-lines))))))

(defn make-frame []
  (seesaw.core/frame
   :title "Test"
   :on-close :exit
   :content (seesaw.core/vertical-panel
             :items [(seesaw.core/horizontal-panel :items [(seesaw.core/slider :id :i1  :min 1 :max 9) (seesaw.core/label :id :t1 :text "0")])
                     (seesaw.core/horizontal-panel :items [(seesaw.core/slider :id :i2  :min 1 :max 9) (seesaw.core/label :id :t2 :text "0")])
                     (seesaw.core/horizontal-panel :items [(seesaw.core/slider :id :i3  :min 1 :max 9) (seesaw.core/label :id :t3 :text "0")])
                     (seesaw.core/horizontal-panel :items [(seesaw.core/slider :id :i4  :min 1 :max 9) (seesaw.core/label :id :t4 :text "0")])
                     (seesaw.core/horizontal-panel :items [(seesaw.core/slider :id :i5  :min 1 :max 9) (seesaw.core/label :id :t5 :text "0")])
                     (seesaw.core/horizontal-panel :items [(seesaw.core/slider :id :i6  :min 1 :max 9) (seesaw.core/label :id :t6 :text "0")])
                     (seesaw.core/horizontal-panel :items [(seesaw.core/slider :id :i7  :min 1 :max 9) (seesaw.core/label :id :t7 :text "0")])
                     (seesaw.core/horizontal-panel :items [(seesaw.core/slider :id :i8  :min 1 :max 9) (seesaw.core/label :id :t8 :text "0")])
                     (seesaw.core/horizontal-panel :items [(seesaw.core/slider :id :i9  :min 1 :max 9) (seesaw.core/label :id :t9 :text "0")])
                     (seesaw.core/horizontal-panel :items [(seesaw.core/slider :id :i10 :min 1 :max 9) (seesaw.core/label :id :t10 :text "0")])
                     (seesaw.core/horizontal-panel :items [(seesaw.core/slider :id :i11 :min 1 :max 9) (seesaw.core/label :id :t11 :text "0")])
                     (seesaw.core/horizontal-panel :items [(seesaw.core/slider :id :i12 :min 1 :max 9) (seesaw.core/label :id :t12 :text "0")])
                     (seesaw.core/horizontal-panel :items [(seesaw.core/slider :id :i13 :min 1 :max 9) (seesaw.core/label :id :t13 :text "0")])
                     (seesaw.core/horizontal-panel :items [(seesaw.core/slider :id :i14 :min 1 :max 9) (seesaw.core/label :id :t14 :text "0")])
                     (seesaw.core/text :id :t-result :multi-line? true :editable? false :text "No result yet")])))

(defn- update-value [root lines]
  (let [{:keys [i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14]} (seesaw.core/value root)
        [result intermediate] (run-prog lines [i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14])]
    (println "We're updating ->" result "\n"  (clojure.string/join \newline (map str intermediate)))
    (seesaw.core/config! (seesaw.core/select root [:#t1]) :text (str i1))
    (seesaw.core/config! (seesaw.core/select root [:#t2]) :text (str i2))
    (seesaw.core/config! (seesaw.core/select root [:#t3]) :text (str i3))
    (seesaw.core/config! (seesaw.core/select root [:#t4]) :text (str i4))
    (seesaw.core/config! (seesaw.core/select root [:#t5]) :text (str i5))
    (seesaw.core/config! (seesaw.core/select root [:#t6]) :text (str i6))
    (seesaw.core/config! (seesaw.core/select root [:#t7]) :text (str i7))
    (seesaw.core/config! (seesaw.core/select root [:#t8]) :text (str i8))
    (seesaw.core/config! (seesaw.core/select root [:#t9]) :text (str i9))
    (seesaw.core/config! (seesaw.core/select root [:#t10]) :text (str i10))
    (seesaw.core/config! (seesaw.core/select root [:#t11]) :text (str i11))
    (seesaw.core/config! (seesaw.core/select root [:#t12]) :text (str i12))
    (seesaw.core/config! (seesaw.core/select root [:#t13]) :text (str i13))
    (seesaw.core/config! (seesaw.core/select root [:#t14]) :text (str i14))
    (seesaw.core/config! (seesaw.core/select root [:#t-result]) :text
                         (clojure.string/join \newline (filter #(clojure.string/starts-with? % "add z y") intermediate)))))

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- show-gui []
  (let [lines (util/read-lines "input_day_24.txt")
        root (make-frame)]
    (seesaw.core/listen (map #(seesaw.core/select root [%]) [:#i1 :#i2 :#i3 :#i4 :#i5 :#i6 :#i7 :#i8 :#i9 :#i10 :#i11 :#i12 :#i13 :#i14]) :change
                        (fn [_]
                          (update-value root lines)))

    (seesaw.core/invoke-later
     (-> root
         seesaw.core/pack!
         seesaw.core/show!))))

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- compare-sections []
  (let [lines (util/read-lines "input_day_24.txt")
        sections (take-nth 2 (rest (partition-by #(= "inp w" %) lines)))
        comparisons (apply mapv vector sections)
        cleaned-up (map #(if (apply = %) [(first %)] %) comparisons)]
    (println (clojure.string/join \newline (map str cleaned-up)))))

(defn day24-precomputed []
  (println "Solution Day 24-1: 29989297949519 (Precomputed since I solved this by hand)")
  (println "Solution Day 24-2: 19518121316118 (Precomputed since I solved this by hand)"))

(defn -main [] (day24-precomputed))