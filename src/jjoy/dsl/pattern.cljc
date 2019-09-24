(ns jjoy.dsl.pattern
  (:require [clojure.string :as str]
            [jjoy.utils :as ut]))

(defn escaping [s]
  (str/replace s #"^(\/([\/\$]))" "$2"))

(defn binding? [p]
  (second (re-find #"^\$(\w+)" p)))

(defn matcher [p]
  (cond
    (map? p)
    (let [matchers (ut/map-vals matcher p)]
      (fn f
        ([x] (f {} x))
        ([matches x]
         (if (map? x)
           (reduce
            (fn [matches [k matcher]]
              (if-let [[_ key-value] (find x k)]
                (if-let [matches' (matcher matches key-value)]
                  matches'
                  (reduced false))
                (reduced false)))
            matches
            matchers)
           false))))

    (sequential? p)
    (let [matchers (map matcher p)]
      (fn f
        ([x] (f {} x))
        ([matches x]
         (if (and (sequential? x)
                  (<= (count matchers) (count x)))
           (reduce
            (fn [matches [matcher x]]
              (if-let [matches' (matcher matches x)]
                matches'
                (reduced false)))
            matches
            (map vector matchers x))
           false))))

    (string? p) (if-let [bind (binding? p)]
                  (if (re-find #"^_" bind)
                    (fn f
                      ([x] (f {} x))
                      ([matches x] matches))
                    (fn f
                      ([x] (f {} x))
                      ([matches x]
                       (if-let [[_ match] (find matches bind)]
                         (if (= match x)
                           matches
                           false)
                         (assoc matches bind x)))))
                  (let [p' (escaping p)]
                    (fn f
                      ([x] (f {} x))
                      ([matches x]
                       (if (= p' x)
                         matches
                         false)))))

    :else (fn f
            ([x] (f {} x))
            ([matches x]
             (if (= p x)
               matches
               false)))))
