(ns jjoy.dsl.query-test
  (:require [jjoy.dsl.query :as query]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest basic
  (let [data {"users" [{"bio" {"name" "Andrew"}}
                       {"bio" {"name" "Petr"}}]
              "key with space" "with-space"
              "dotted.key" "dotted"
              "parens[]key" "parens"}]
    (t/are [query res] (= res
                          (query/run query data))
      "users" [{"bio" {"name" "Andrew"}}
               {"bio" {"name" "Petr"}}]
      "users.bio" [{"name" "Andrew"}
                   {"name" "Petr"}]
      "unknown" nil
      "users.unknown" [nil nil]
      "users[0].bio.name" "Andrew"
      "" data
      ;; escaping
      "key with space" "with-space"
      "dotted\\.key" "dotted"
      "parens\\[]key" "parens")))
