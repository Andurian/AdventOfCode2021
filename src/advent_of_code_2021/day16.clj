(ns advent-of-code-2021.day16
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string])
  (:gen-class))

(def hexmap {\0 "0000"
             \1 "0001"
             \2 "0010"
             \3 "0011"
             \4 "0100"
             \5 "0101"
             \6 "0110"
             \7 "0111"
             \8 "1000"
             \9 "1001"
             \A "1010"
             \B "1011"
             \C "1100"
             \D "1101"
             \E "1110"
             \F "1111"})

(defn ->binary [input]
  (clojure.string/join (map hexmap input)))

(defn ->decimal [binary-string]
  (Long/parseLong binary-string 2))

(declare parse-package)

(defn parse-literal [input]
  (loop [current-packs []
         i 0]
    (let [pack-str (subs input (* i 5) (* (inc i) 5))
          first-bit (first pack-str)
          value-str (subs pack-str 1)]
      (if (= first-bit \0)
        {:value (->decimal (clojure.string/join (conj current-packs value-str)))
         :remaining (subs input (* 5 (inc i)))}
        (recur (conj current-packs value-str) (inc i))))))

(defn parse-operator [input]
  (let [parse-operator-num-bits
        (fn [input]
          (let [num-bits (->decimal (subs input 0 15))
                remaining-input (subs input 15)
                initial-input-length (count remaining-input)]
            (loop [current-str remaining-input
                   packages []]
              (if (>= (- initial-input-length (count current-str)) num-bits)
                {:value packages :remaining current-str}
                (let [package (parse-package current-str)]
                  (recur (:remaining package) (conj packages package)))))))
        parse-operator-num-packages
        (fn [input]
          (let [num-packages (->decimal (subs input 0 11))
                remaining-input (subs input 11)]
            (loop [current-str remaining-input
                   packages []]
              (if (>= (count packages) num-packages)
                {:value packages :remaining current-str}
                (let [package (parse-package current-str)]
                  (recur (:remaining package) (conj packages package)))))))]
    (if (= \0 (first input))
      (parse-operator-num-bits (subs input 1))
      (parse-operator-num-packages (subs input 1)))))

(defn parse-package [input]
  (let [version (->decimal (subs input 0 3))
        type (->decimal (subs input 3 6))
        value
        (if (= type 4)
          (parse-literal (subs input 6))
          (parse-operator (subs input 6)))]
    {:version version
     :type type
     :value (:value value)
     :remaining (:remaining value)}))

(defn sum-version-numbers
  ([input] (sum-version-numbers input 0))
  ([input current]
   (if (vector? (:value input))
     (apply + (concat [current (:version input)] (map sum-version-numbers (:value input))))
     (+ current (:version input)))))

(defn calc-value [package]
  (let [type (:type package)
        value (:value package)]
    (if (= 4 type)
      value
      (let [contained-values (map calc-value value)]
        (cond
          (= 0 type) ; sum
          (apply + contained-values)
          (= 1 type) ; product
          (apply * contained-values)
          (= 2 type) ; min
          (apply min contained-values)
          (= 3 type) ; max
          (apply max contained-values)
          (= 5 type) ; greater
          (if (> (first contained-values) (last contained-values)) 1 0)
          (= 6 type) ; less
          (if (< (first contained-values) (last contained-values)) 1 0)
          (= 7 type) ; equal
          (if (= (first contained-values) (last contained-values)) 1 0))))))

(defn task01 [input]
  (let [package (parse-package (->binary input))]
    (println "Sum of all version numbers:" (sum-version-numbers package))))

(defn task02 [input]
  (let [package (parse-package (->binary input))]
    (println "Expression evaluates to:" (calc-value package))))

(defn day16
  ([] (println "Using default input")
      (day16 "input_day_16.txt"))
  ([filename]
   (let [input (first (read-lines filename))]
     (time (task01 input));
     (time (task02 input));
     )))

(defn -main [] (day16))