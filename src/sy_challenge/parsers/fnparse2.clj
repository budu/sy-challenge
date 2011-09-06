
(ns sy-challenge.parsers.fnparse2
  "Monadic implementation of the Shunting-yard algorithm. This version
  include most C boolean and numeric operators."
  (:use clojure.template
        name.choi.joshua.fnparse
        sy-challenge.core))

;;;; State

(defstruct state :remainder :op-stack :out-stack)

(defn initial-state [tokens]
  (struct state tokens '() '()))

;;;; State Helpers

(defn peek-info [key]
  (semantics (get-info key) #(peek %)))

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

(def <lp> (lit \())
(def <rp> (lit \)))

(def <uinteger>
  (semantics
   (rep+ (lit-alt-seq "0123456789"))
   #(->> % (apply str) Integer/parseInt)))

(def <integer>
  (semantics
   (conc (opt <sign>) <uinteger>)
   (fn [[s i]] (if s (list s i) i))))

(def <symbol>
  (semantics
   (rep+ (lit-alt-seq "abcdefghijklmnopqrstuvwxyz"))
   #(->> % (apply str) symbol)))

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
  [<or>   "||" 2 :l 'or]
  [<and>  "&&" 2 :l 'and]
  [<bor>  "|"  2 :l 'bit-or]
  [<bxor> "^"  2 :l 'bit-xor]
  [<band> "&"  2 :l 'bit-and]
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

(def <sy-token> (alt <sy-operand> <sy-operator>))

(def <expression>
  (complex [_ (conc <sy-token>
                    <separator>
                    (opt <expression>)
                    (shift-op (constantly true)))
            out (get-info :out-stack)]
    (first out)))

;;;; Parser

(defn parse-error [state message]
  (throw
   (Exception.
    (format "%s at line %s, column %s: %s"
            message
            (:line state)
            (:column state)
            (apply str (:remainder state))))))

(defmethod parse :fnparse2
  [_ s]
  (rule-match
   <expression>
   #(parse-error % "Fail")
   #(parse-error %2 "Incomplete")
   (initial-state s)))
