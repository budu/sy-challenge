
(ns sy-challenge.parsers.fnparse
  "Monadic implementation of the Shunting-yard algorithm. Simple version
  including only 3 level of left-to-right binary operators"
  (:use clojure.template
        name.choi.joshua.fnparse
        sy-challenge.core))

;;;; State

(defstruct state :remainder :op-stack :out-stack :priority)

(defn initial-state [tokens]
  (struct state tokens '() '() nil))

;;;; State Helpers

(defn set-priority [p]
  (update-info :priority (constantly p)))

(defn peek-info [key]
  (semantics (get-info key) #(peek %)))

(defn pop-info [key]
  (semantics (update-info key #(next %)) first))

(defn push-info [key val]
  (update-info key #(conj % val)))

;;;; Basic Literals

(def <separator> (rep* (lit \space)))

(def <integer>
  (semantics
   (rep+ (lit-alt-seq "0123456789"))
   #(->> %
         (apply str)
         Integer/parseInt)))

;;;; Operators

(do-template [sym c p]
  (def sym (invisi-conc (lit c) (set-priority p)))
  <eq> \= 0
  <add> \+ 1
  <sub> \- 1
  <mul> \* 2
  <div> \/ 2)

(def <op> (alt <eq> <add> <sub> <mul> <div>))

;;;; Shunting-yard implementation

(defn ->sexp [op]
  (complex [s (pop-info :out-stack)
            f (pop-info :out-stack)
            :let [args (list f s)]
            _ (push-info :out-stack (->> op str symbol (conj args)))]
    nil))

(defn shift-op [pred]
  (opt
   (complex [op (peek-info :op-stack)
             :let [[op p] op]
             :when (and p (pred p))
             _ (conc (pop-info :op-stack)
                     (->sexp op)
                     (shift-op pred))]
     nil)))

(def <sy-operand>
  (complex [s <integer>
            :let [s s]
            _ (push-info :out-stack s)]
    nil))

(def <sy-operator>
  (complex [o1 <op>
            p1 (get-info :priority)
            :let [o1 o1, p1 p1]
            _ (conc (shift-op #(<= p1 %))
                    (push-info :op-stack [o1 p1]))]
    nil))

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

(defmethod parse :fnparse
  [_ s]
  (rule-match
   <expression>
   #(parse-error % "Fail")
   #(parse-error %2 "Incomplete")
   (initial-state s)))
