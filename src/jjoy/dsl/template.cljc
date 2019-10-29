(ns jjoy.dsl.template
  (:require [jjoy.dsl.query :as query]
            [jjoy.dsl.pattern :as pattern]
            [jjoy.utils :as ut]
            [clojure.pprint :refer [cl-format]]
            [jjoy.base :as base])
  (:refer-clojure :exclude [compile]))

(defn compile-query [q]
  (let [query (query/compile q)]
    (fn [[data]]
      (query data))))

(defn compile-parent-query [n q]
  (let [query (query/compile q)]
    (fn [stack]
      (query (nth stack n)))))

(declare compile)

(defmulti compile-op (fn [op _args] op))

(defmethod compile-op "format" [_ [format-string & queries]]
  (let [format-queries (mapv query/compile queries)]
    (fn [[data]]
      (apply cl-format nil format-string (map #(% data) format-queries)))))

(defmethod compile-op "for" [_ [q sub-template]]
  (let [query (query/compile q)
        sub-template' (compile sub-template)]
    (fn [[data :as stack]]
      (let [items (query data)]
        (for [i items]
          (sub-template' (cons i stack)))))))

(defmethod compile-op "a" [_ sub-templates]
  (let [sub-templates' (map compile sub-templates)]
    (fn [data]
      (for [t sub-templates']
        (t data)))))

(defmethod compile-op "call" [_ expr]
  (fn [stack]
    (first (:stack (base/run stack expr)))))

(defmethod compile-op "when" [_ [query expr sub-template]]
  (let [query' (query/compile query)
        sub-template' (compile sub-template)]
    (fn [[data :as stack]]
      (when (first (:stack (base/run (cons (query' data) stack) expr)))
        (sub-template' stack)))))

(defmethod compile-op "match" [_ [pattern sub-template]]
  (let [matcher (pattern/matcher pattern)
        sub-template' (compile sub-template)]
    (fn [[data :as stack]]
      (when-let [bindings (matcher data)]
        (sub-template' (cons bindings stack))))))

(defmethod compile-op "merge" [_ sub-templates]
  (let [sub-templates' (map compile sub-templates)]
    (fn [data]
      (reduce #(ut/deep-merge %1 (%2 data)) {} sub-templates'))))

(defn compile [template]
  (cond
    (map? template)
    (let [compiled (ut/map-vals compile template)]
      (fn [stack]
        (ut/map-vals #(% stack) compiled)))

    (sequential? template)
    (let [[op & args] template]
      (cond
        (= \. (nth op 0)) (compile-query (subs op 1))
        (= \^ (nth op 0)) (let [[_ parents _query] (re-find #"(\^+)(.+)" op)
                                n (count parents)]
                            (compile-parent-query n (subs op n)))
        :else (compile-op op args)))

    :else (constantly template)))

(defn substitute [compiled data]
  (compiled (list data)))

(defn run [template data]
  (substitute (compile template) data))

(comment
  (run {"foo" {"bar" [".user.name"]}}
    {"user" {"name" "Andrew"}})

  (run {"names" ["for" "users"
                 {"first-name" [".name"]}]}
    {"users" [{"name" "Andrew"}
              {"name" "Kirill"}]})

  (run {"names" ["for" "users"
                 {"category" ["^category"]
                  "first-name" [".name"]}]}
    {"category" "admin"
     "users" [{"name" "Andrew"}]})

  (run ["format" "Hey, ~a" "name"]
    {"name" "Andrew"})

  (run {"range" ["a"
                 [".first"]
                 {"type" "int" "value" [".last"]}]}
    {"first" 1 "last" 5}))
