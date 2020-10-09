(ns jjoy.dsl.query
  (:require [clojure.string :as str])
  (:refer-clojure :exclude [compile]))

(defn deescape [s]
  (str/replace s #"\\(\.|\\|\[)" "$1"))

(defn index-key [k]
  (when (= \[ (nth k 0))
    (#?(:clj Long/parseLong :cljs js/parseInt) (subs k 1 (dec (count k))))))

(defn parse [query]
  (->> (re-seq #"(([^\.\\\[]|\\[\\\.\[])+)(\[[^\]]+\])?" query)
       (map (fn [[_ key _ ?index]]
              (if ?index
                {:type :indexed-key
                 :key (deescape key)
                 :index (index-key ?index)}
                {:type :key
                 :key (deescape key)})))))

(defn compile*
  [[{:keys [type key] :as sub-query} & query]]
  (if sub-query
    (let [sub (compile* query)]
      (case type
        :key
        (fn [data]
          (let [v (get data key)]
            (cond
              (sequential? v) (map sub v)
              :else (sub v))))

        :indexed-key
        (let [index (:index sub-query)]
          (fn [data]
            (let [v (get data key)]
              (when (and (sequential? v)
                         (< index (count v)))
                (sub (nth v index))))))))
    identity))

(defn compile [s]
  (compile* (parse s)))

(defn run [s data]
  ((compile s) data))
