(ns jjoy.core-test
  (:require [jjoy.core :as jj]
            [clojure.test :refer [deftest is are testing]]))

(require 'matcher-combinators.test)
(require 'jjoy.test-utils)

(defn main-run [program]
  (get-in (-> program
              (jj/load-program)
              (jj/run))
          [:threads 0 :stack]))

(deftest basic
  (let [program {"definitions"
                 {"dub" {"type" "words"
                         "body" [(jj/word "dup") (jj/word "+")]}
                  "dup" {"type" "words"
                         "body" ["a-aa" (jj/word "shuffle")]}
                  "+" {"type" "ff"
                       "name" "clojure.core/+"
                       "arity" 2}}
                 "body" [3 (jj/word "dub")]}]
    (is (= [6]
           (main-run program)))))

(deftest clojure-definitions
  (let [program {"definitions" {"str.concat" {"type" "clojure"
                                              "arity" 2
                                              "fn" (pr-str '(fn [a b] (str a b)))}}
                 "body" ["foo" "bar" (jj/word "str.concat")]}]
    (is (= ["foobar"]
           (main-run program)))))

(deftest instructions
  (let [program {"definitions"
                 {"+" {"type" "ff"
                       "name" "clojure.core/+"
                       "arity" 2}

                  "incr"
                  {"type" "instruction"
                   "fn" "(let [plus #word \"+\"]
                           (fn [{:keys [r-stack stack]}]
                             {:stack (cons 1 stack)
                              :r-stack (cons plus r-stack)}))"}}
                 "body" [2 (jj/word "incr")]}]
    (is (= [3]
           (main-run program))))

  (testing "map combinator"
    (let [program {"definitions"
                   {"+" {"type" "ff"
                         "name" "clojure.core/+"
                         "arity" 2}

                    "map-acc"
                    {"type" "instruction"
                     "fn" "(fn [{[res & stack] :stack
                                 [acc & r-stack] :r-stack}]
                             {:stack stack
                              :r-stack (concat [#word \"map-step\" (conj acc res)] r-stack)})"}

                    "map-step"
                    {"type" "instruction"
                     "fn"
                     "(fn [{:keys [stack]
                            [acc [x & source] p & r-stack] :r-stack}]
                        (if x
                          {:stack (cons x stack)
                           :r-stack (concat p [#word \"map-acc\" acc source p] r-stack)}
                          {:stack (cons acc stack)
                           :r-stack r-stack}))"}

                    "map"
                    {"type" "instruction"
                     "fn" "(fn [{[p l & stack] :stack :keys [r-stack]}]
                             {:stack stack
                              :r-stack (concat [#word \"map-step\" [] l p] r-stack)})"}}

                   "body" [[1 2 3] [1 (jj/word "+")] (jj/word "map")]}]
      (is (= [[2 3 4]]
             (main-run program))))))


#_(deftest prelude
    (are [body expected] (= expected (main-run (jj/jsonify+load {:body body})))
      '(1 2) [2 1]
      '(1 2 swap) [1 2]
      '(3 5 2 [+] dip /) [4]
      '(1 2 over) [1 2 1]
      '(1 [2 +] keep) [1 3]
      '(3 2 1 rot) [3 1 2]

      '([1] 2 prefix) [[2 1]]
      '([1] 2 suffix) [[1 2]]
      '([1 2 3] 1 nth) [2]
      '(vec) [[]]

      '(true "ok" "fail" ?) ["ok"]
      '(false "ok" "fail" ?) ["fail"]
      '(true ["ok"] ["fail"] if) ["ok"]
      '(true [1] when) [1]

      '([1 2 3] rest) [[2 3]]
      '(1 [2] curry) [[1 2]]
      '(5 dup [dup 1 >] [1 - dup [*] dip] while drop) [120]
      '(0 [1 2 3] [+] each) [6]

      '([1 2 3] [1 +] map) [[2 3 4]]
      '({"a" 1 "b" 2} seq [1 nth 1 +] map) [[2 3]]))

(comment
  (jj/run (jj/jsonify+load
           {:body '([1] [1 +] map)}))
  )

(deftest spawn+consume
  (let [program {"definitions"
         {"+" {"type" "ff"
               "name" "clojure.core/+"
               "arity" 2}
          "incrementer"
          {"type" "words"
           "body" [[1 (jj/word "+")] (jj/word "spawn")]}}
         "body" [[3] (jj/word "incrementer") "a-[a]" (jj/word "shuffle") (jj/word "consume")]}]
    (is (= [1 [4]]
           (main-run program))))

  ;; (let [program (jj/jsonify+load
  ;;                '{:vocabulary {incrementer ([1] swap prefix [+] spawn)}
  ;;                  :body (3 incrementer join)})]
  ;;   (is (match? [[4]]
  ;;               (main-run program))))

  ;; (let [program (jj/jsonify+load
  ;;                '{:vocabulary {incrementer (panic!)}
  ;;                  :body (3 incrementer join)})]
  ;;   (is (match? [(jj/word "panic")]
  ;;               (main-run program))))
  )

#_(deftest pmap-test
    (let [program (jj/jsonify+load
                   '{:vocabulary {incrementer (1 +)}
                     :body ([1 2 3] [incrementer] pmap)})]
      (is (match? [[2 3 4]]
                  (main-run program)))))

(deftest unparks+serialization
  (let [program (jj/load-program
                 {"definitions"
                  {"+" {"type" "ff"
                        "name" "clojure.core/+"
                        "arity" 2}}
                  "body" [3 (jj/word "yield") (jj/word "+")]})
        state (jj/run program)
        res (jj/unpark program state 0 [2])
        res+serialization (jj/load-state program (jj/dump-state (jj/unpark program state 0 [2])))]
    (is (= {:next-thread-id 1, :threads {0 {:stack [5], :r-stack nil}}}
           res res+serialization))))

#_(deftest query
    (let [program (jj/jsonify+load
                   '{:body ({:foo 1} "foo" query)})]
      (is (match? [1]
                  (main-run program)))))

#_(deftest template
    (let [program (jj/jsonify+load
                   '{:body ({:foo 1} {:bar [".foo"]} template)})]
      (is (match? [{"bar" 1}]
                  (main-run program)))))
