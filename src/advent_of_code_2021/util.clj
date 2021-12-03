(ns advent-of-code-2021.util
  (:require clojure.java.io)
  (:require clojure.string))

(defn read-lines [filename]
  (let [file  (.getFile (clojure.java.io/resource filename))
        content (slurp file)]
    (clojure.string/split content #"\r\n")))

(defn read-numbers [filename]
  (vec (map read-string (read-lines filename))))