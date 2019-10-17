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

;; (defn results-run [program]
;;   (get-in (human/run program) [:results 0]))

(defn body-parse [body]
  (get (human/parse body) "body"))

(t/deftest basic
  (t/is (= [2 1 "•+"] (body-parse "2 1 +")))
  (t/testing "object"
    (t/is (= [{"foo" 2}]
             (body-parse "{foo 2}")
             (body-parse "{\"foo\" 2}")
             (body-parse "{\"foo\": 2}")))

    (t/is (= [{"foo" 2
               "bar" 3}]
             (body-parse "{foo 2 bar 3}")
             (body-parse "{foo: 2, bar: 3}"))))

  (t/testing "array"
    (t/is (= [[1 2 3]]
             (body-parse "[1 2 3]")
             (body-parse "[ 1 2 3 ]")
             (body-parse "[ 1, 2 3 ]"))))

  ;; (t/testing "comments"
  ;;   (t/is (= [1]
  ;;            (body-parse "# hey\n 1"))))

  ;; (t/testing "word pragma"
  ;;   (t/is (= [3]
  ;;            (results-run "1 2 #word \"+\""))))

  ;; (t/testing "ignore pragma"
  ;;   (t/is (= [3]
  ;;            (results-run "1 #. ololo 2 +"))))
  )


(t/deftest vocabulary
  (t/is (= {"•inc" [1 "•+"]
            "•dec" [1 "•-"]}
           (get (human/parse "#def inc [1 +] #def dec [1 -]") "vocabulary"))))

(t/deftest imports
  (t/is (= #{{"word" "•json.encode",
              "imported-word" "•_imports/codecs/json.encode/2",
              "alias" "codecs",
              "function" "json.encode",
              "arity" 2}}
           (get (human/parse "#import {\"codecs/json.encode/2\" json.encode}")
                "imports"))))

(t/deftest uses
  (t/is (= ["•utils/incrementer/inc"
            "•utils/incrementer/inc"
            "•utils/incrementer/inc"]
           (get (human/parse
                 "#use [utils/incrementer [utils/incrementer as incr refer all]]
                  utils/incrementer/inc incr/inc inc"
                 {:fs (human/in-memory-fs {"utils/incrementer" "#def inc [1 +]"})})
                "body")))

  (t/is (= {"imports" #{},
            "vocabulary" {"•lib1/inc" [1], "•foo" ["•lib2/inc"]},
            "body" ["•lib1/inc"]}
           (human/parse
            "#use [[lib1 refer [inc]]]
             #def foo [#use [[lib2 refer [inc]]] inc]
             inc"
            {:fs (human/in-memory-fs {"lib1" "#def inc [1]"
                                      "lib2" "#def inc [2]"})}))))

(t/deftest custom-pragmas
  (t/testing "default"
    (t/is (= [["foo" "bar"] "•template"]
             (body-parse "#template [foo bar]")))))
