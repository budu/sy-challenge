
(ns sy-challenge.parsers.logging
  (:use clojure.contrib.monads))

(defn logged-update-val
  [key f]
  (fn [s]
    (let [old-val (get s key)
          new-s (assoc s key (f old-val))]
      (println "*********************************"
               "updating " key "\n"
               "remainder: " (:remainder s) "\n"
               "op-stack:  " (:op-stack s)
               (if (= key :op-stack) (str "=> " (:op-stack new-s)) "") "\n"
               "out-stack: " (:out-stack s)
               (if (= key :out-stack) (str "=> " (:out-stack new-s)) "") "\n"
               "priority:  " (:priority s)
               (if (= key :priority) (str "=> " (:priority new-s)) "") "\n")
      [old-val new-s])))

(defmacro log-state-on-update
  [& body]
  `(binding [update-val logged-update-val]
     ~@body))
