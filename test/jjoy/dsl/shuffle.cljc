(ns jjoy.dsl.shuffle
  (:require [jjoy.dsl.shuffle :as shuffle]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest basic
  (t/are [data shuffle res] (= res
                               (shuffle/run shuffle data))
    [1 2] "ab-ba" [2 1]
    [1 [2 3 4]] "a[b..c]-a..cb" [1 3 4 2]
    [[[1 2 3]]] "[[a..b]]-ab" [1 [2 3]]))
