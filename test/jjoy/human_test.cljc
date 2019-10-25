(ns jjoy.human-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [jjoy.core :as jj]
   [jjoy.human :as human]))

(defn to-core [program options]
  (-> program
      (human/parse options)
      (human/to-core)))

(defn main-run* [program]
  (-> program
      (to-core {})
      (jj/load-program)
      (jj/run)
      (get-in [:threads 0 :stack])))

(defmacro h [& forms]
  (apply pr-str forms))

(defmacro main-run [& forms]
  `(main-run* ~(apply pr-str forms)))

(deftest basic
  (is (= [[3] 1 2] (main-run 2 1 [3]))))

(deftest defclj
  (is (= [3]
         (main-run
          :defclj + [a b] (+ a b)
          1 2 +))))

(deftest imports
  (is (= [4]
         (main-run
          :import {[clojure.core/+ 2] +
                   [clojure.core/- 2] -}
          1 5 2 - +))))

(deftest instructions
  (is (= [[2 3 4]]
         (main-run
          :declare map-step
          :definstruction map-acc
          [{[res & stack] :stack
            [acc & r-stack] :r-stack}]
          {:stack stack
           :r-stack (concat [{::human/word map-step} (conj acc res)] r-stack)}

          :definstruction map-step
          [{:keys [stack]
            [acc [x & source] p & r-stack] :r-stack}]
          (if x
            {:stack (cons x stack)
             :r-stack (concat p [{::human/word map-acc} acc source p] r-stack)}
            {:stack (cons acc stack)
             :r-stack r-stack})

          :definstruction map [{[p l & stack] :stack :keys [r-stack]}]
          {:stack stack
           :r-stack (concat [{::human/word map-step} [] l p] r-stack)}

          :import
          {[clojure.core/+ 2] +}

          [1 2 3] [1 +] map))))

(deftest clj
  (is (= [10]
         (main-run
          2 3 :clj [a b] (* (+ a b) 2)))))

(deftest uses
  (is (= ["•utils.numbers/one"
          "•utils.numbers/one"
          "•utils.numbers/one"]
         (get (to-core
               (h
                :use [utils.numbers [utils.numbers :as numbers :refer :all]]
                utils.numbers/one numbers/one one)
               {:fs (human/in-memory-fs {'utils.numbers ":def one [1]"})})
              "body")))

  (is (= {"definitions"
          {"lib1/inc" {"type" "words", "body" [1]},
           "lib2/inc" {"type" "words", "body" [2]},
           "main/foo" {"type" "words", "body" ["•lib2/inc"]}},
          "body" ["•lib1/inc"]}
         (to-core
          (h
           :use [[lib1 :refer [inc]]]
           :def foo [:use [[lib2 :refer [inc]]] inc]
           inc)
          {:fs (human/in-memory-fs {'lib1 ":def inc [1]"
                                    'lib2 ":def inc [2]"})}))))

(deftest default-keywords
  (is (= ["body" "•query"]
         (-> (h :query body)
             (to-core {})
             (get "body")))))

(deftest special-words
  (is (= ["•yield"]
         (-> (h yield)
             (to-core {})
             (get "body")))))
