(ns advent-of-code-2021.core
  (:require
   [advent-of-code-2021.day01 :refer [day01]]
   [advent-of-code-2021.day02 :refer [day02]]
   [advent-of-code-2021.day03 :refer [day03]]
   [advent-of-code-2021.day04 :refer [day04]]
   [advent-of-code-2021.day05 :refer [day05]]
   [advent-of-code-2021.day06 :refer [day06]]
   [advent-of-code-2021.day07 :refer [day07]]
   [advent-of-code-2021.day08 :refer [day08]]
   [advent-of-code-2021.day09 :refer [day09]]
   [advent-of-code-2021.day10 :refer [day10]]
   [advent-of-code-2021.day11 :refer [day11]]
   [advent-of-code-2021.day12 :refer [day12]]
   [advent-of-code-2021.day13 :refer [day13]]
   [advent-of-code-2021.day14 :refer [day14]]
   [advent-of-code-2021.day15 :refer [day15]]
   [advent-of-code-2021.day16 :refer [day16]]
   [advent-of-code-2021.day17 :refer [day17-precomputed]]
   [advent-of-code-2021.day18 :refer [day18]]
   [advent-of-code-2021.day19 :refer [day19]]
   [advent-of-code-2021.day20 :refer [day20]]
   [advent-of-code-2021.day21 :refer [day21]]
   [advent-of-code-2021.day22 :refer [day22]]
   [advent-of-code-2021.day23 :refer [day23-precomputed]]
   [advent-of-code-2021.day24 :refer [day24-precomputed]]
   [advent-of-code-2021.day25 :refer [day25]])
  (:gen-class))

(defn -main []
  (println "Executing all days")
  (time (do
          (day01)
          (day02)
          (day03)
          (day04)
          (day05)
          (day06)
          (day07)
          (day08)
          (day09)
          (day10)
          (day11)
          (day12)
          (day13)
          (day14)
          (day15)
          (day16)
          (day17-precomputed)
          (day18)
          (day19)
          (day20)
          (day21)
          (day22)
          (day23-precomputed)
          (day24-precomputed)
          (day25))))