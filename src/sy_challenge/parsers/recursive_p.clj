
(ns sy-challenge.parsers.recursive-p
  "Recursive implementation of Shunting-yard algorithm, based on:
  https://gist.github.com/953966"
  (:use sy-challenge.core))

;;;; Tokenizer

(defmacro -!>
  ([x form]
     `(-> ~x ~form))
  ([x form & forms]
     `(let [i# (-!> ~x ~form)]
        (or i# (-!> ~x ~@forms)))))

(defmacro deftoken [name re & forms]
  `(defn ~name [s#]
     (when-let [[m# sp#] (re-find ~(re-pattern
                                    (str "^(" re ")\\s*")) s#)]
       [m# (->> sp# ~@forms)])))

(deftoken <lp> #"\(" first)

(deftoken <rp> #"\)" first)

(deftoken <integer> #"\d+" Integer/parseInt)

(deftoken <symbol> #"\w+" symbol)

(deftoken <ops> #"[=+\-*/]" symbol)

(defn tokenize-error [remaining]
  (throw
   (Exception.
    (format "No matching token for '%s'" remaining))))

(defn tokenizer [s]
  (let [remainder (atom s)]
    (fn [& options]
      (if (= :remainder (first options))
        @remainder
        (let [[match token] (-!> @remainder
                                 <lp> <rp> <ops>
                                 <integer>
                                 <symbol>)]
          (swap! remainder #(.substring % (count match)))
          (if (or (empty? @remainder) match)
            token
            (tokenize-error @remainder)))))))

;;;; Parser

(defprotocol parser-p
  (pop-info [this key])
  (push-info [this key val])
  (lower? [this op])
  (->sexp [this op])
  (shift-op [this op])
  (step [this])
  (parse* [this]))

(defrecord Parser [tokenizer out-stack op-stack ops]
  parser-p

  (pop-info [this key]
    [(peek (key this))
     (assoc this key (next (key this)))])

  (push-info [this key val]
    (assoc this key (conj (key this) val)))

  (lower? [this op]
    (<= (or (ops op) 0)
        (or (ops (peek op-stack)) 0)))

  (->sexp [this op]
    (let [[s parser] (pop-info this :out-stack)
          [f parser] (pop-info parser :out-stack)]
      (push-info parser :out-stack (list op f s))))

  (shift-op [this op]
    (if (lower? this op)
      (let [[op2 parser] (pop-info this :op-stack)]
        (if op2
          (-> (->sexp parser op2)
              (shift-op op))
          parser))
      this))

  (step [this]
    (let [token (tokenizer)]
      (if (number? token)
        (push-info this :out-stack token)
        (-> this
            (shift-op token)
            (push-info :op-stack token)))))

  (parse* [this]
    (if (empty? (tokenizer :remainder))
      (->> (iterate #(shift-op % nil) this)
           (take (inc (count op-stack)))
           last
           :out-stack
           first)
      (parse* (step this)))))

(def *ops* {'= 0 '+ 1 '- 1 '* 2 '/ 2})

(defn make-parser [tokenizer & [ops]]
  (Parser. tokenizer '() '() ops))

(defmethod parse :recursive-p
  [_ s]
  (parse* (make-parser (tokenizer s) *ops*)))
