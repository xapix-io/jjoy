(ns jjoy.human-test
  (:require [jjoy.human :as human]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(defn results-run [program]
  (get-in (human/run program) [:results 0]))

(t/deftest basic
  (t/is (= [3] (results-run "2 1 +")))
  (t/testing "object"
    (t/is (= [{"foo" 2}]
             (results-run "{foo 2}")
             (results-run "{\"foo\" 2}")
             (results-run "{\"foo\": 2}")))

    (t/is (= [{"foo" 2
               "bar" 3}]
             (results-run "{foo 2 bar 3}")
             (results-run "{foo: 2, bar: 3}"))))

  (t/testing "array"
    (t/is (= [[1 2 3]]
             (results-run "[1 2 3]")
             (results-run "[ 1 2 3 ]")
             (results-run "[ 1, 2 3 ]")))))

(t/deftest vocabulary
  (t/is (= [4]
           (results-run "#def dub dup + ### 2 dub")
           (results-run "#def dub dup + ### 2 dub"))))

