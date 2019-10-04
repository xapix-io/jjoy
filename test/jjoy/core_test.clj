(ns jjoy.core-test
  (:require [jjoy.core :as jj]
            [clojure.test :refer [deftest is are]]))

(require 'matcher-combinators.test)

(defn main-run [program]
  (get-in (jj/run program)
          [:threads 0 :stack]))

(deftest basic
  (let [program (jj/load-program
                 {"vocabulary" {(jj/word "dub") [(jj/word "dup") (jj/word "+")]}
                  "body" [3 (jj/word "dub")]})]
    (is (= [6]
           (main-run program)))))

(deftest form-program
  (let [program (jj/jsonify+load
                 '{:vocabulary {dub (dup +)}
                   :body (3 dub)})]
    (is (= [6]
           (main-run program)))))

(deftest prelude
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

    '([1 2 3] [1 +] map) [[2 3 4]]))

(comment
  (jj/run (jj/jsonify+load
           {:body '([1] [1 +] map)}))
  )

(deftest spawn+consume
  (let [program (jj/jsonify+load
                 '{:vocabulary {incrementer ([1] swap prefix [+] spawn)}
                   :body (3 incrementer "a-[a]" shuffle
                            consume drop)})]
    (is (match? [[4]]
                (main-run program))))

  (let [program (jj/jsonify+load
                 '{:vocabulary {incrementer ([1] swap prefix [+] spawn)}
                   :body (3 incrementer join)})]
    (is (match? [[4]]
                (main-run program))))

  (let [program (jj/jsonify+load
                 '{:vocabulary {incrementer (panic!)}
                   :body (3 incrementer join)})]
    (is (match? [(jj/word "panic")]
                (main-run program)))))

(deftest pmap-test
  (let [program (jj/jsonify+load
                 '{:vocabulary {incrementer (1 +)}
                   :body ([1 2 3] [incrementer] pmap)})]
    (is (match? [[2 3 4]]
                (main-run program)))))

(deftest unparks
  (let [program (jj/jsonify+load
                 '{:body (3 yield +)})
        state (jj/run program)
        res (jj/unpark program state 0 [2])]
    (is (match? {:threads {0 {:stack [5]}}} res))))

(deftest query
  (let [program (jj/jsonify+load
                 '{:body ({:foo 1} "foo" query)})]
    (is (match? [1]
                (main-run program)))))

(deftest template
  (let [program (jj/jsonify+load
                 '{:body ({:foo 1} {:bar [".foo"]} template)})]
    (is (match? [{"bar" 1}]
                (main-run program)))))
