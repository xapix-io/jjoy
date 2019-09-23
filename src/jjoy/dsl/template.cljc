(ns jjoy.dsl.template
  (:require [jjoy.dsl.query :as query]
            [jjoy.utils :as ut]
            [clojure.pprint :refer [cl-format]])
  (:refer-clojure :exclude [compile]))

(defn compile-format [[format-string & queries]]
  (let [format-queries (mapv query/compile queries)]
    (fn [[data]]
      (apply cl-format nil format-string (map #(% data) format-queries)))))

(defn compile-query [q]
  (let [query (query/compile q)]
    (fn [[data]]
      (prn "---Q" q data (query data))
      (query data))))

(defn compile-parent-query [n q]
  (let [query (query/compile q)]
    (fn [stack]
      (query (nth stack n)))))

(declare compile)

(defn compile-for [[q sub-template]]
  (let [query (query/compile q)
        sub-template' (compile sub-template)]
    (fn [[data :as stack]]
      (let [items (query data)]
        (for [i items]
          (sub-template' (cons i stack)))))))

(defn compile [template]
  (cond
    (map? template)
    (let [compiled (ut/map-vals compile template)]
      (fn [stack]
        (ut/map-vals #(% stack) compiled)))

    (sequential? template)
    (let [[op & args] template]
      (cond
        (= "format" op) (compile-format args)
        (= \. (nth op 0)) (compile-query (subs op 1))
        (= \^ (nth op 0)) (let [[_ parents query] (re-find #"(\^+)(.+)" op)
                                n (count parents)]
                            (compile-parent-query n (subs op n)))
        (= "for" op) (compile-for args)))

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
