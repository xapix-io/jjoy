(ns jjoy.core-test
  (:require [jjoy.core :as jj]
            [clojure.test :refer [deftest is are]]))

(require 'matcher-combinators.test)

(deftest basic
  (let [program (jj/load-program
                 {"vocabulary" {(jj/word "dub") [(jj/word "dup") (jj/word "+")]}
                  "body" [3 (jj/word "dub")]})]
    (is (match? {:results {0 [6]}}
                (jj/run program)))))

(deftest form-program
  (let [program (jj/jsonify+load
                 '{:vocabulary {dub (dup +)}
                   :body (3 dub)})]
    (is (match? {:results {0 [6]}}
                (jj/run program)))))

(deftest prelude
  (are [body expected] (= {0 expected} (:results (jj/run (jj/jsonify+load
                                                          {:body body}))))
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
    '(5 dup [dup 1 >] [1 - dup [*] dip] while drop) [120]))

(comment
  (jj/run (jj/jsonify+load
           {:body '(5 [dup prn 1 - dup 0 >] loop )}))
  )

(deftest spawn+join
  (let [program (jj/jsonify+load
                 '{:vocabulary {incrementer ([1] swap prefix [+] spawn)}
                   :body (3 incrementer join)})]
    (is (match? {:results {0 [4]}}
                (jj/run program)))))

#_(deftest multi-spawn+reduce
  (let [program (jj/jsonify+load
                 '{:vocabulary {incrementer ([1] swap prefix [+] spawn)}
                   :body (3 incrementer join)})]
    (is (match? {:results {0 [4]}}
                (jj/run program)))))
