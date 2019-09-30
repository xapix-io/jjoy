(ns jjoy.human-test
  #?@
   (:clj
    [(:require
      [clojure.test :as t]
      [jjoy.base :as jj]
      [jjoy.human :as human])]
    :cljs
    [(:require
      [cljs.test :as t :include-macros true]
      [jjoy.base :as jj]
      [jjoy.human :as human])]))

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
             (results-run "[ 1, 2 3 ]"))))

  (t/testing "word pragma"
    (t/is (= [3]
             (results-run "1 2 #word \"+\""))))

  (t/testing "ignore pragma"
    (t/is (= [3]
             (results-run "1 #. ololo 2 +")))))

(t/deftest vocabulary
  (t/is (= [4]
           (results-run "#defs {dub: [dup +]} 2 dub"))))

(t/deftest imports
  (t/is (= {(jj/word "json.encode") {"alias" "codecs",
                                     "function" "json.encode",
                                     "arity" 2}}
           (get (human/parse "#import {\"codecs.json.encode/2\" json.encode}")
                "imports"))))


