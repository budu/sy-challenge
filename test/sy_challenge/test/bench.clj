(ns sy-challenge.test.bench
  (:use [sy-challenge.core] :reload)
  (:use [clojure.test :exclude [report]]
        criterium.core))

(defn parsers []
  (->> "src/sy_challenge/parsers/"
       java.io.File.
       .listFiles
       (map #(->> %
                  .getName (re-find #"(.*).clj$")
                  second))))

(def *parsers* nil)

(defn with-parsers-fixture [f]
  (let [ps (parsers)]
    (->> ps
         (map #(str "sy-challenge.parsers." %))
         (map symbol)
         (apply require))
    (binding [*parsers* (map #(.replace % \_ \-) ps)]
      (f))))

(use-fixtures :once
  #(with-progress-reporting (%))
  with-parsers-fixture)

(defmacro bench-parser [p s]
  (let [form `(parse (keyword ~p) ~s)]
    `(do
       (println "Testing" ~p "...")
       (when (is (eval ~form) "Should be true")
         (println "Benchmarking" ~p "...")
         (bench ~form)))))

(defmacro defbench [n s]
  `(deftest ~n
     (println ~(.getName n))
     (doseq [parser# *parsers*]
       (bench-parser parser# ~s))))

(defbench simple "1 + 2 = 3")
