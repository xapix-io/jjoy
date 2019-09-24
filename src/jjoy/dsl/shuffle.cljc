(ns jjoy.dsl.shuffle
  (:require [instaparse.core :as insta]
            [jjoy.utils :as ut])
  (:refer-clojure :exclude [compile]))

(def grammar"
  <RULE> = LHS <'-'> RHS

  LHS = LHSEL+
  <LHSEL> = LHSNESTED | BINDING
  LHSNESTED = <'['> LHSEL* REST? <']'>

  RHS = RHSEL*
  <RHSEL> = RHSNESTED | BINDING | REST
  RHSNESTED = <'['> RHSEL+ <']'>

  REST = <'..'> BINDING_RAW
  BINDING = BINDING_RAW
  <BINDING_RAW> = #'[a-z]'
")

(def parser
  (insta/parser grammar))

;; hey, let's write it in a better way!
(defn compile-lhs [{:keys [path bindings-map] :as acc} [el & elements]]
  (if el
    (let [set-binding (fn [path]
                        (let [[_ binding] el]
                          (when (get bindings-map binding)
                            (throw (ex-info (str "Duplicate binding " binding {:binding binding}))))
                          (assoc bindings-map binding path)))
          [t & args] el]
      (case t
        :BINDING
        (let [[binding] args]
          (recur (assoc acc
                        :bindings-map (set-binding path)
                        :path (update path (dec (count path)) inc))
                 elements))
        :LHSNESTED
        (recur (assoc (compile-lhs (update acc :path conj 0) args)
                      :path (update path (dec (count path)) inc))
               elements)
        :REST
        (assoc acc
               :bindings-map (set-binding (assoc path (dec (count path)) {:from (last path)})))))
    acc))

(defn compile-rhs [extractors [el & els]]
  (if el
    (let [next (compile-rhs extractors els)
          get-binding (fn []
                        (let [[_ binding] el]
                          (or (get extractors binding)
                              (throw (ex-info (str "Unknown binding in RHS " binding) {:binding binding})))))
          [t] el]
      (case t
        :BINDING
        (let [ex (get-binding)] (fn [data] (cons (ex data) (next data))))

        :RHSNESTED
        (let [[_ & nested-els] el
              nested-els' (compile-rhs extractors )]
          (fn [data] (cons (nested-els' data) (next data))))

        :REST
        (let [ex (get-binding)] (fn [data] (concat (ex data) (next data))))))
    (constantly ())))

(defn path->extractor [[fragment & path]]
  (cond
    (nil? fragment) identity
    (:from fragment) (let [n (:from fragment)]
                       #(drop n %))
    :else (let [next-extractor (path->extractor path)]
            (fn [data]
              (next-extractor (nth data fragment))))))

(defn compile
  ([s] (compile s {:reverse? false}))
  ([s {:keys [reverse?]}]
   (let [[[_ & lhs] [_ & rhs]] (parser s)
         lhs' (compile-lhs {:path [0] :bindings-map {}} lhs)
         bindings (:bindings-map lhs')
         max-index (->> (vals bindings)
                        (map first)
                        (apply max))
         bindings (if reverse?
                    (ut/map-vals (fn [[i & path]]
                                   (cons (- max-index i) path))
                                 bindings)
                    bindings)
         extractors (ut/map-vals path->extractor bindings)
         rhs' (compile-rhs extractors rhs)]
     (fn [data]
       (if (<= (count data) max-index)
         (throw (ex-info "Not enough elements" {:expected (inc max-index)
                                                :actual (count data)}))
         (rhs' data))))))

(defn run
  ([s data] (run s data {}))
  ([s data options]
   ((compile s options) data)))

(comment
  (parser "ab-ba")
  (parser "[a..c]-..ca")
  ((compile "[a]b-b") [[1] 2])
  ((compile "[a..c]b-b..ca") [[1 2 3 4] 5])
  ((compile "[a..b]-..b") [[1 2 3]])
  )


;; (defn parse-lhs [s]
;;   ())

;; (defn parse [s]
;;   (let [[_ lhs rhs] (re-find #"^([a-z\[\]\.]+)-([a-z]*)$" s)]
;;     ))
