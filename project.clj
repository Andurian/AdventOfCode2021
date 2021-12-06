(defproject advent_of_code_2021 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :main ^:skip-aot advent-of-code-2021.core
  :target-path "target/%s"
  :profiles {:day01 {:main advent-of-code-2021.day01}
             :day02 {:main advent-of-code-2021.day02}
             :day03 {:main advent-of-code-2021.day03}
             :day04 {:main advent-of-code-2021.day04}
             :day05 {:main advent-of-code-2021.day05}
             :day06 {:main advent-of-code-2021.day06}
             :uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :aliases {"day01" ["with-profile" "day01" "run"];
            "day02" ["with-profile" "day02" "run"];
            "day03" ["with-profile" "day03" "run"];
            "day04" ["with-profile" "day04" "run"];
            "day05" ["with-profile" "day05" "run"];
            "day06" ["with-profile" "day06" "run"];
            })
