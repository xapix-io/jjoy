(ns jjoy.dsl.template-test
  (:require [jjoy.dsl.template :as template]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest basic
  (let [data {"users" [{"name" "Andrew"}
                       {"name" "Kirill"}]}]
    (t/are [template res] (= res
                             (template/run template data)))))
