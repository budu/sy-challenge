(ns sy-challenge.core)

;; (def *ops*)

;; (defmacro with-ops [ops & body]
;;   `(binding [*ops* ops]
;;      ~@body))

(defmulti parse
  (fn [p _]
    (require (->> p name (str "sy-challenge.parsers.") symbol))
    p))
