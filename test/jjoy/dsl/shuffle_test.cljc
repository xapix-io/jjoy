(ns jjoy.dsl.shuffle-test
  (:require [jjoy.dsl.shuffle :as shuffle]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest basic
  (t/are [data shuffle res] (= res
                               (shuffle/run shuffle data))
    [1 2] "ab-ba" [2 1]
    [1] "a-aa" [1 1]
    [1 [2 3 4]] "a[b..c]-a..cb" [1 3 4 2]
    [[[1 2 3]]] "[[a..b]]-ab" [1 [2 3]]

    [1 2 3] "ab-ba" [2 1 3]
    [1 2 3] "a-" [2 3]
    [[1 2] 3] "ab-[b..a]" [[3 1 2]]))

(t/deftest reverse-test
  (t/are [data shuffle res] (= res
                               (shuffle/run shuffle data {:reverse? true}))
    (list 1 2 3) "ab-ba" [2 1 3]
    (list 3 2 1) "abc-abca" [1 3 2 1]))


;; "a[b..c]-a..cb"
;; (defn foo [a [b & c]]
;;   (concat [a] c [b]))
