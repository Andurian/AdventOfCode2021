(ns advent-of-code-2021.day24
  (:require [advent-of-code-2021.util :as util]
            [clojure.string :as str]
            [seesaw.core])
  (:gen-class)
  (:use [seesaw core color border]))


(def initial-state {\w 0
                    \x 0
                    \y 0
                    \z 0})

(defn inp [state a stream]
  [(assoc state (first a) (first stream)) (rest stream)])

(defn binary-op [op state a b]
  (let [val-a (get state (first a))
        val-b (if (some #(= (first b) %) (keys initial-state))
                (get state (first b))
                (Integer/parseInt b))
        res (op val-a val-b)]
    (assoc state (first a) res)))

(defn add [state a b]
  (binary-op + state a b))

(defn mul [state a b]
  (binary-op * state a b))

(defn div [state a b]
  (binary-op quot state a b))

(defn my-mod [state a b]
  (binary-op mod state a b))

(defn eql [state a b]
  (binary-op (fn [v1 v2] (if (= v1 v2) 1 0)) state a b))

(defn exec-line [line state stream]
  (let [[instruction op1 op2] (str/split line #" ")]
    (case instruction
      "inp" (inp state op1 stream)
      "add" [(add state op1 op2) stream]
      "mul" [(mul state op1 op2) stream]
      "div" [(div state op1 op2)  stream]
      "mod" [(my-mod state op1 op2) stream]
      "eql" [(eql state op1 op2) stream])))

(defn run-prog [lines initial-stream]
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

;; (defn run-prog-from-file [filename initial-stream]
;;   (let [lines (util/read-lines filename)]
;;     (run-prog lines initial-stream)))

;; (defn find-biggest-monad [filename]
;;   (let [lines (util/read-lines filename)]
;;     (loop [test-num (vec (repeat 14 9))
;;            current-idx 0]
;;       (println "Testing:" test-num)
;;       (let [state (run-prog lines test-num)]
;;         (println "Restult:" state "\n")
;;         (if (= 0 (get state \z))
;;           test-num
;;           (let [current-idx-value (nth test-num current-idx)
;;                 next-idx-value (nth test-num (inc current-idx))]
;;             (if (= 1 (nth test-num current-idx))
;;               (recur (assoc test-num (inc current-idx) (dec next-idx-value)) (inc current-idx))
;;               (recur (assoc test-num current-idx (dec current-idx-value)) current-idx))))))))

;; (defn digits [n]
;;   (->> n
;;        (iterate #(quot % 10))
;;        (take-while pos?)
;;        (mapv #(mod % 10))
;;        rseq))

;; (defn find-biggest-monad-brute-force [filename]
;;   (let [lines (util/read-lines filename)]
;;     (loop [test-num-int 99999999999999]
;;       (let [test-num (digits test-num-int)]
;;         (if (not (some #(= 0 %) test-num))
;;           (do
;;             (println "Testing:" test-num)
;;             (let [state (run-prog lines test-num)]
;;               (println "Result:" state)
;;               (if (= 0 (get state \z))
;;                 test-num-int
;;                 (recur (dec test-num-int)))))
;;           (recur (dec test-num-int)))))))

;; (def test-stream [1 3 5 7 9 2 4 6 8 9 9 9 9 9])

(defn make-frame2 []
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

(defn update-value [root lines]
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

(defn make-frame []
  (seesaw.core/frame
   :title "Slider Example"
   :on-close :exit
   :content
   (seesaw.core/horizontal-panel :items [(seesaw.core/vertical-panel :items ["<html>
          Slide the sliders to change<br>
          the color to the right</html>"
                                                                             (seesaw.core/slider :id :red   :min 0 :max 255)
                                                                             (seesaw.core/slider :id :green :min 0 :max 255)
                                                                             (seesaw.core/slider :id :blue  :min 0 :max 255)])
                                         (seesaw.core/canvas :id :canvas :border (seesaw.border/line-border) :size [200 :by 200])])))

(defn update-color [root]
  (let [{:keys [red green blue]} (seesaw.core/value root)] ; <- Use (value) to get map of values
    (seesaw.core/config! (seesaw.core/select root [:#canvas])
                         :background (seesaw.color/color red green blue))))



(defn day24-old []
  (let [root (make-frame)]
    (seesaw.core/listen (map #(seesaw.core/select root [%]) [:#red :#green :#blue]) :change
                        (fn [e]
                          (update-color root)))

    (seesaw.core/invoke-later
     (-> (make-frame2)
         seesaw.core/pack!
         seesaw.core/show!))))

(defn day24-gui []
  (let [lines (util/read-lines "input_day_24.txt")
        root (make-frame2)]
    (seesaw.core/listen (map #(seesaw.core/select root [%]) [:#i1 :#i2 :#i3 :#i4 :#i5 :#i6 :#i7 :#i8 :#i9 :#i10 :#i11 :#i12 :#i13 :#i14]) :change
                        (fn [e]
                          (update-value root lines)))

    (seesaw.core/invoke-later
     (-> root
         seesaw.core/pack!
         seesaw.core/show!))))

(defn transpose [m]
  (apply mapv vector m))

(defn day24 []
  (let [lines (util/read-lines "input_day_24.txt")
        sections (take-nth 2 (rest (partition-by #(= "inp w" %) lines)))
        comparisons (transpose sections)
        cleaned-up (map #(if (apply = %) [(first %)] %) comparisons)]
    (println (clojure.string/join \newline (map str cleaned-up)))))

(defn -main [] (day24-gui))