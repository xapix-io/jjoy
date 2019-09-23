(ns jjoy.base
  (:require [jjoy.lib :as lib]
            [jjoy.utils :as ut]))

(def ^:dynamic *vocabulary*)

(def word-prefix \•)

(defn word [n]
  (str word-prefix n))

(defn word? [s]
  (and (string? s)
       (= (first s) word-prefix)))

(defmacro binary-op [[a b] body]
  `(fn [{[~b ~a & stack#] :stack :as s#}]
     (assoc s# :stack (cons ~body stack#))))

(defmacro ternary-op [[a b c] body]
  `(fn [{[~c ~b ~a & stack#] :stack :as s#}]
     (assoc s# :stack (cons ~body stack#))))

(defmacro unary-op [[a] body]
  `(fn [{[~a & stack#] :stack :as s#}]
     (assoc s# :stack (cons ~body stack#))))

(def primitive
  {(word "call") (fn [{:keys [p-stack]
                       [q & stack] :stack :as s}]
                   (assoc s
                          :stack stack
                          :p-stack (concat q p-stack)))
   (word "dip") (fn [{:keys [p-stack]
                      [P a & stack] :stack :as s}]
                  (assoc s
                         :stack stack
                         :p-stack (concat P [a] p-stack)))
   (word "dup") (fn [{[q :as stack] :stack :as s}]
                  (assoc s :stack (cons q stack)))
   (word "swap") (fn [{[a b & stack] :stack :as s}]
                   (assoc s :stack (cons b (cons a stack))))
   (word "drop") (fn [{[a & stack] :stack :as s}]
                   (assoc s :stack stack))
   (word "over") (fn [{[y x :as stack] :stack :as s}]
                   (assoc s :stack (cons x stack)))
   (word "rot") (fn [{[z y x & stack] :stack :as s}]
                  (assoc s :stack (cons x (cons z (cons y stack)))))
   (word "?" ) (fn [{[z y x & stack] :stack :as s}]
                 (if x
                   (assoc s :stack (cons y stack))
                   (assoc s :stack (cons z stack))))

   (word "+") (binary-op [a b] (+ a b))
   (word "-") (binary-op [a b] (- a b))
   (word "*") (binary-op [a b] (* a b))
   (word "/") (binary-op [a b] (/ a b))
   (word "<") (binary-op [a b] (< a b))
   (word ">") (binary-op [a b] (> a b))
   (word "=") (binary-op [a b] (= a b))

   (word "vec") (fn [{:keys [stack] :as s}]
                  (assoc s :stack (cons [] stack)))
   (word "nth") (binary-op [v i] (nth v i))
   (word "concat") (binary-op [a b] (vec (concat a b)))
   (word "length") (unary-op [a] (count a))
   (word "subvec") (ternary-op [v start end] (subvec v start end))
   (word "prefix") (binary-op [v x] (vec (cons x v)))
   (word "suffix") (binary-op [v x] (conj v x))

   (word "prn") (fn [{[x & stack] :stack :as s}]
                  (prn x)
                  (assoc s :stack stack))})

(defn json-values-walk [f v]
  (cond
    (map? v) (into {} (map (fn [[k v]] [(f k) ((partial json-values-walk f) v)]) v))
    (sequential? v) (mapv (partial json-values-walk f) v)
    :else (f v)))

(defn make-definition [body]
  (fn [{:keys [p-stack] :as s}]
    (assoc s :p-stack (concat body p-stack))))

(defn jsonify [form]
  (json-values-walk (fn [v] (cond
                              (symbol? v) (word (str v))
                              (keyword? v) (name v)
                              :else v)) form))

(def lib (ut/map-vals make-definition (jsonify lib/lib)))

(defn tick
  [{[term & p-stack] :p-stack
    :keys [stack] :as s}]
  (cond
    (nil? term) (dissoc s :p-stack)

    (word? term)
    (if-let [d (get *vocabulary* term)]
      (do
        (d (assoc s :p-stack p-stack)))
      (throw (ex-info (str "Unknown word " term) {:type ::unknown-word
                                                  :word term})))

    :else (assoc s
                 :stack (cons term stack)
                 :p-stack p-stack)))

(defn run
  [p-stack]
  (loop [s (tick {:stack ()
                  :p-stack p-stack})]
    (if (:p-stack s)
      (recur (tick s))
      s)))
