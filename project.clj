(defproject advent_of_code_2021 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [seesaw "1.5.0"]]
  :main ^:skip-aot advent-of-code-2021.core
  :target-path "target/%s"
  :profiles {:day01 {:main advent-of-code-2021.day01}
             :day02 {:main advent-of-code-2021.day02}
             :day03 {:main advent-of-code-2021.day03}
             :day04 {:main advent-of-code-2021.day04}
             :day05 {:main advent-of-code-2021.day05}
             :day06 {:main advent-of-code-2021.day06}
             :day07 {:main advent-of-code-2021.day07}
             :day08 {:main advent-of-code-2021.day08}
             :day09 {:main advent-of-code-2021.day09}
             :day10 {:main advent-of-code-2021.day10}
             :day11 {:main advent-of-code-2021.day11}
             :day12 {:main advent-of-code-2021.day12}
             :day13 {:main advent-of-code-2021.day13}
             :day14 {:main advent-of-code-2021.day14}
             :day15 {:main advent-of-code-2021.day15}
             :day16 {:main advent-of-code-2021.day16}
             :day17 {:main advent-of-code-2021.day17}
             :day18 {:main advent-of-code-2021.day18}
             :day19 {:main advent-of-code-2021.day19}
             :day20 {:main advent-of-code-2021.day20}
             :day21 {:main advent-of-code-2021.day21}
             :day22 {:main advent-of-code-2021.day22}
             :day23 {:main advent-of-code-2021.day23}
             :day24 {:main advent-of-code-2021.day24}
             :day25 {:main advent-of-code-2021.day25}
             :uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :aliases {"day01" ["with-profile" "day01" "run"];
            "day02" ["with-profile" "day02" "run"];
            "day03" ["with-profile" "day03" "run"];
            "day04" ["with-profile" "day04" "run"];
            "day05" ["with-profile" "day05" "run"];
            "day06" ["with-profile" "day06" "run"];
            "day07" ["with-profile" "day07" "run"];
            "day08" ["with-profile" "day08" "run"];
            "day09" ["with-profile" "day09" "run"];
            "day10" ["with-profile" "day10" "run"];
            "day11" ["with-profile" "day11" "run"];
            "day12" ["with-profile" "day12" "run"];
            "day13" ["with-profile" "day13" "run"];
            "day14" ["with-profile" "day14" "run"];
            "day15" ["with-profile" "day15" "run"];
            "day16" ["with-profile" "day16" "run"];
            "day17" ["with-profile" "day17" "run"];
            "day19" ["with-profile" "day19" "run"];
            "day20" ["with-profile" "day20" "run"];
            "day21" ["with-profile" "day21" "run"];
            "day22" ["with-profile" "day22" "run"];
            "day23" ["with-profile" "day23" "run"];
            "day24" ["with-profile" "day24" "run"];
            "day25" ["with-profile" "day25" "run"];
            })
