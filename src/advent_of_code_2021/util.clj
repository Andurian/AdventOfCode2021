(ns advent-of-code-2021.util
  (:require [clojure.java.io]
            [clojure.string]))

(defn read-lines [filename]
  (let [file (clojure.java.io/resource filename)
        content (slurp file)]
    (clojure.string/split content #"\r\n")))

(defn read-numbers [filename]
  (vec (map read-string (read-lines filename))))

(defmacro get-time [expr]
  `(let [start# (. System (currentTimeMillis))
         ret# ~expr]
     [(double (- (. System (currentTimeMillis)) start#))
      ret#]))