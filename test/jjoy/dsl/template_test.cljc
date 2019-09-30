(ns jjoy.dsl.template-test
  (:require [jjoy.dsl.template :as template]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [jjoy.base :as base]))

(t/deftest basic
  (let [data {"category" "admin"
              "users" [{"name" "Andrew"}
                       {"name" "Kirill"}]}]
    (t/are [template res] (= res
                             (template/run template data))
      {"foo" {"bar" [".users[0].name"]}}
      {"foo" {"bar" "Andrew"}}

      {"names" ["for" "users"
                {"first-name" [".name"]}]}
      {"names" [{"first-name" "Andrew"}
                {"first-name" "Kirill"}]}

      {"names" ["for" "users"
                {"category" ["^category"]
                 "first-name" [".name"]}]}
      {"names" [{"first-name" "Andrew"
                 "category" "admin"}
                {"first-name" "Kirill"
                 "category" "admin"}]}

      ["format" "Hey, ~a" "users[0].name"]
      "Hey, Andrew"

      ["a" [".users[0].name"] [".category"]]
      ["Andrew" "admin"]

      ["merge"
       {"foo" {"zoo" 2}}
       {"foo" {"bar" 3}}]
      {"foo" {"zoo" 2 "bar" 3}}

      ["when" "category" ["admin" (base/word "=")]
       "ok!"]
      "ok!"

      ["when" "category" ["user"  (base/word "not")]
       "ok!"]
      nil

      ["match" {"users" ["$_" {"name" "$name"}]}
       {"first-name" [".name"]}]
      {"first-name" "Kirill"}

      ["match" {"users" ["$_" "$_" {"name" "$name"}]}
       {"first-name" [".name"]}]
      nil

      ["call" 2 1 (base/word "+")]
      3)))

;; TODO
(comment
  ;; two form query
  [".users[0]"
   {"first-name" [".name"]}]

  ;; for nested loops
  ;; {"users" [{"likes" ["bananas" "books"]}]}
  ["for" "users" "likes"
   {"like" "."}]

  ;; for operators when/match
  ["for" "users" ["when" "name" ["Andrew" (base/word "=")]]
   {"first-name" [".name"]}])
