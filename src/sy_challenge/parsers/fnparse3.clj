
(ns sy-challenge.parsers.fnparse3
  "Monadic implementation of the Shunting-yard algorithm, including
  infix binary operator with associativity, function calls and basic
  sequence support. It transalate a simple C like expression syntax into
  s-expression that can be evaluated by Clojure.

    1 + 2                   =>  (+ 1 2)
    1 + 2 * 3               =>  (+ 1 (* 2 3))
    1 * 2 + 3               =>  (+ (* 1 2) 3)
    (1 + 2) * 3             =>  (* (identity (+ 1 2)) 3)
    1 + 2 * 3 = 1 * 2 + 3   =>  (= (+ 1 (* 2 3)) (+ (* 1 2) 3))
    min(1, 1 + 1, 3) < 2    =>  (< (min 1 (+ 1 1) 3) 2)
    [1, 2++, true && false] =>  (vector (- 1 2) (inc 3) (and true false))

  It has some issues in its current form:

   * It confuse +/- operators with number signs (which have higher
     priority) when there's no space between between the operator and
     the number.
   * No distinction between postfix and infix increment/decrement operators.
   * The logical and/or operators have higher precedence their bitwise
     counterparts."
  (:use clojure.template
        name.choi.joshua.fnparse
        sy-challenge.core))

;;;; State

(defstruct state :remainder :op-stack :out-stack :priority)

(defn initial-state [tokens]
  (struct state tokens '() '() nil))

;;;; State Helpers

(defn println-info [key]
  (semantics (get-info key) #(println %)))

(defn peek-info [key]
  (semantics (get-info key) #(first %)))

(defn pop-info [key]
  (semantics (update-info key #(next %)) first))

(defn take-info [key n]
  (semantics (update-info key #(drop n %)) #(take n %)))

(defn push-info [key & vals]
  (update-info key #(apply conj % vals)))

;;;; Tokens

(def <sign>
  (semantics
   (lit-alt-seq "+-")
   (comp symbol str)))

(def <separator> (rep* (lit \space)))

(def <list-separator> (lit \,))

(def <lp> (lit \())
(def <rp> (lit \)))

(def <lb> (lit \[))
(def <rb> (lit \]))

(def <uinteger>
  (semantics
   (rep+ (lit-alt-seq "0123456789"))
   #(->> % (apply str) Integer/parseInt)))

(def <integer>
  (semantics
   (conc (opt <sign>) <uinteger>)
   (fn [[s i]] (if s (list s i) i))))

(def <word>
  (semantics
   (rep+ (lit-alt-seq "abcdefghijklmnopqrstuvwxyz"))
   #(apply str %)))

(def <symbol>
  (semantics
   (rep+ (conc <word> (opt (lit \-))))
   #(->> % flatten (apply str) symbol)))

;;;; Operators

(defn partition-ops [ops p]
  (map #(conj % p) (partition 5 ops)))

(defmacro def-ops [& ops]
  (let [ops-info (mapcat partition-ops ops (range))
        ops (map second ops-info)]
    `(do
       (do-template [~'p ~'name ~'op ~'n ~'a ~'s]
         (def ~'name
           (semantics 
            (invisi-conc (lit-conc-seq ~'op)
                         (set-info :op-info
                                   {:priority ~'p
                                    :n ~'n
                                    :assoc ~'a
                                    :symbol ~'s}))
            ~'s))
         ~@(apply concat ops-info))
       (def ~'<op> (alt ~@(reverse ops))))))

(def-ops
  [<bor>  "|"  2 :l 'bit-or]
  [<bxor> "^"  2 :l 'bit-xor]
  [<band> "&"  2 :l 'bit-and]
  [<or>   "||" 2 :l 'or]
  [<and>  "&&" 2 :l 'and]
  [<eq>   "="  2 :l '=]
  [<lt>   "<"  2 :l '<
   <gt>   ">"  2 :l '>
   <lte>  "<=" 2 :l '<=
   <gte>  ">=" 2 :l '>=]
  [<sl>   "<<" 2 :l 'bit-shift-left
   <sr>   ">>" 2 :l 'bit-shift-right]
  [<add>  "+"  2 :l '+
   <sub>  "-"  2 :l '-]
  [<mul>  "*"  2 :l '*
   <div>  "/"  2 :l '/
   <mod>  "%"  2 :l 'mod]
  [<inc>  "++" 1 :r 'inc
   <dec>  "--" 1 :r 'dec
   <not>  "!"  1 :r 'not])

;;;; Shunting-yard implementation

(defn ->sexp [op oi]
  (complex [args (take-info :out-stack (:n oi))
            :let [args (reverse args)]
            _ (push-info :out-stack (conj args (:symbol oi)))]
    nil))

(defn shift-op [pred]
  (opt
   (complex [op (peek-info :op-stack)
             :let [[op oi] op]
             :when (and oi (pred oi))
             _ (conc (pop-info :op-stack)
                     (->sexp op oi)
                     (shift-op pred))]
     nil)))

(def <sy-operand>
  (complex [t (alt <integer> <symbol>)
            :let [sp t]
            _ (push-info :out-stack sp)]
    sp))

(def <sy-operator>
  (complex [o1 <op>
            i1 (get-info :op-info)
            :let [o1 o1, i1 i1]
            _ (conc (shift-op #((if (= :l (:assoc %)) <= <)
                                (:priority i1)
                                (:priority %)))
                    (push-info :op-stack [o1 i1]))]
    o1))

(def <sy-function>
  (complex [t (invisi-conc <symbol>
                           (followed-by
                            (conc <separator> <lp>)))
            :let [sp t]
            _ (push-info :op-stack sp)]
    sp))

(def shift-item*
  (complex [args (peek-info :out-stack)
            :let [args args]
            :when (vector? args)
            _ (pop-info :out-stack)]
    args))

(def shift-item
  (opt
   (complex [arg (pop-info :out-stack)
             args (opt shift-item*)
             :let [arg arg args args]
             _ (push-info :out-stack (if (and args (vector? args))
                                       (conj args arg)
                                       arg))]
     nil)))

(defn list-start? [op]
  (and (vector? op)
       (or (= (first op) \()
           (= (first op) \[))))

(def list-separator
  (complex [t <list-separator>
            :let [sp t]
            _ (conc (shift-op (comp not list-start?))
                    shift-item)]
    nil))

(def list-start
  (complex [t (alt <lp> <lb>)
            :let [sp t]
            _ (conc (push-info :op-stack [sp nil])
                    (push-info :out-stack []))]
    nil))

(defn function-end [& [s]]
  (opt
   (complex [f (peek-info :op-stack)
             :let [f f]
             :when (not (list-start? f))
             args (pop-info :out-stack)
             :let [args args]
             _ (conc (pop-info :op-stack)
                     (push-info :out-stack (concat [(or f s 'identity)]
                                                   args)))]
     nil)))

(def list-end
  (complex [t (alt <rp> <rb>)
            _ (shift-op (comp not list-start?))
            m (pop-info :op-stack)
            :let [sp t [m _] m]
            :when (= ({\( \) \[ \]} m) sp)
            _ (conc shift-item
                    (function-end (when (= sp \]) 'vector)))]
    nil))

(def <sy-token>
  (alt <sy-function>
       <sy-operand>
       list-separator
       <sy-operator>
       list-start
       list-end))

(def <expression>
  (complex [_ (conc <sy-token>
                    <separator>
                    (opt <expression>)
                    (shift-op (constantly true)))
            out (get-info :out-stack)]
    (conj (reverse out) 'do)))

;;;; Parser

(defn parse-error [state message]
  (throw
   (Exception.
    (format "%s at line %s, column %s: %s"
            message
            (:line state)
            (:column state)
            (apply str (:remainder state))))))

(defmethod parse :fnparse3
  [_ s]
  (rule-match
   <expression>
   #(parse-error % "Fail")
   #(parse-error %2 "Incomplete")
   (initial-state s)))
