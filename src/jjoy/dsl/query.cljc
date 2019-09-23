(ns jjoy.dsl.query
  (:require [clojure.string :as str])
  (:refer-clojure :exclude [compile]))

(defn deescape [s]
  (str/replace s #"\\(\.|\\)" "$1"))

(defn parse [query]
  (->> (re-seq #"([^\.\\]|\\[\\\.])+" query)
       (map (fn [[key]] (deescape key)))))

(defn compile*
  [[key & query]]
  (if key
    (let [sub (compile* query)]
      (fn [data]
        (let [v (get data key)]
          (cond
            (sequential? v) (map sub v)
            :else (sub v)))))
    identity))

(defn compile [s]
  (compile* (parse s)))

(defn run [s data]
  ((compile s) data))
