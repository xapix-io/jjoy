(ns jjoy.human
  (:require [clojure.edn :as edn]
            [jjoy.core :as jj]
            [instaparse.core :as insta]
            [jjoy.base :as base]
            [jjoy.utils :as ut]
            [clojure.string :as str])
  (:refer-clojure :exclude [read]))

(defprotocol FS
  (read-path [this path]))

(defn in-memory-fs [m]
  (reify FS
    (read-path [_ path] (get m path))))

(def init-env (->> (for [[k] (concat jj/primitives base/primitives)]
                     [k {:type :primitive
                         :fully-qualified k}])
                   (into {})))

(def grammar "
  BODY = EXPR*
  <EXPR> = WHITESPACE* (NUMBER|STRING|ARRAY|OBJECT|NULL|TRUE|FALSE|WORD) WHITESPACE*
  WORD = #'[-+]' | #'[^-\\s\\[\\]{};\"\\d+:,][^\\s\\[\\]{};\":,]*' | ESCAPED_WORD
  <ESCAPED_WORD> = <'#'> STRING
  ARRAY = BRACKET_OPEN BODY BRACKET_CLOSE
  COMMENT = ONE_LINE_COMMENT | MULTI_LINE_COMMENT
  ONE_LINE_COMMENT = #'# [^\\n]*' | #'#\\n|\\z'
  MULTI_LINE_COMMENT = '##' (#'.' | '\\n')+ '##'
  <BRACKET_OPEN> = <'['>
  <BRACKET_CLOSE> = <']'>
  <WHITESPACE> = <#'[\\s:,]+'>
  KEY_VALUE_PAIR = WHITESPACE* (STRING | WORD) WHITESPACE+ EXPR WHITESPACE*
  OBJECT = CURLY_OPEN KEY_VALUE_PAIR* CURLY_CLOSE
  <CURLY_OPEN> = <'{'>
  <CURLY_CLOSE> = <'}'>
  NUMBER = #'-?(0|([1-9][0-9]*))(\\.[0-9]+)?([eE][+-]?[0-9]+)?'
  STRING = #'\"([^\"\\\\]|(\\\\[\"\\\\nfnrt]))*\"'

  NULL = <'null'>
  TRUE = <'true'>
  FALSE = <'false'>
  ")
;; STRING = #'\"[^\"]+\"'
;; STRING = #'\"([^\"\\]|\\([\"\\/nfnrt])|(u[0-9a-fA-F]{4}))*\"'
;; #\"([^\"\\]|\\([\"\\/nfnrt])|(u[0-9a-fA-F]{4}))*\"'
;; #"-?(0|([1-9][0-9]*))(\\.[0-9]+)?([eE][+-]?[0-9]+)?"

(def reader*
  (insta/parser grammar))

#_(case word
    "word" (do (assert (string? e'))
               (jj/word e'))
    "defs" (do (set-defs e')
               ::ignore)
    "import" (do (set-imports e')
                 ::ignore)
    "." ::ignore)

(declare read-expr read-body)

(defn read-keypair [[_ [k-type k] v]]
  [(case k-type
     :WORD k
     :STRING (edn/read-string k))
   (read-expr v)])

(defn read-expr [[type & xs]]
  (case type
    :TRUE true
    :FALSE false
    :NULL nil
    (:NUMBER :STRING) (edn/read-string (first xs))
    :WORD (jj/word (first xs))
    :OBJECT (into {} (map read-keypair xs))
    :ARRAY (let [[body] xs] (read-body body))))

(defn read-body [[_ & exprs]]
  (mapv read-expr exprs))

(defn read [s]
  (-> s (reader*) (read-body)))

(defn pragma? [s]
  (and (base/word? s)
       (= (second s) \#)))

(defn resolve-word [{:keys [env]} word]
  (or (get-in env [word :fully-qualified])
      (assert (contains? env word) (str "Unknown word " (base/unword word)))))

(declare analyze*)

(defn handle-import [ctx specs]
  (let [specs' (map (fn [[spec word]]
                      (let [_ (assert (base/word? word))
                            [_ alias fun arity] (re-find #"(.+?)/(.+)/(\d+)" spec)]
                        {"word" word
                         "imported-word" (jj/word (str "_imports/" alias "/" fun "/" arity))
                         "alias" alias
                         "function" fun
                         "arity" (edn/read-string arity)}))
                    specs)]
    (-> ctx
        (update :imports into specs')
        (update :env merge (->> (for [{:strs [word imported-word] :as x} specs']
                                  [word {:type :import
                                         :fully-qualified imported-word
                                         :import x}])
                                (into {}))))))

(defn load-lib [{:keys [fs libs-cache]} path]
  (if-let [lib (get @libs-cache path)]
    lib
    (let [content (read-path fs path)
          _ (assert content (str "Unknown lib by path " path))]
      (let [res (analyze* {:fs fs
                           :libs-cache libs-cache
                           :state :consume
                           :env init-env
                           :imports #{}}
                          (read content))]
        (swap! libs-cache assoc path res)
        res))))

(defn handle-use* [ctx spec]
  (let [[path options]
        (cond
          (base/word? spec)
          [(base/unword spec) {"as" spec}]

          (sequential? spec)
          (let [[path-word & args] spec]
            [(base/unword path-word)
             (into {} (map (fn [[k v]] [(base/unword k) v])
                           (partition 2 args)))]))

        {:keys [imports definitions] :as lib} (load-lib ctx path)
        definitions' (->>
                      (for [[w def] definitions]
                        [(jj/word (str path "/" (base/unword w)))
                         (assoc def :library path)])
                      (into {}))
        mapping (cond->
                    {}
                  (get options "as")
                  (into
                   (for [[w {:keys [name]}] definitions']
                     [(jj/word (str (base/unword (get options "as")) "/"
                                    (base/unword name)))
                      w]))

                  (get options "refer")
                  (into
                   (if (= (get options "refer") (jj/word "all"))
                     (for [[w {:keys [name]}] definitions']
                       [name w])
                     (for [w (get options "refer")]
                       (do (assert (get definitions w) (str "Unknown word " w " in library " path))
                           [w (jj/word (str path "/"
                                            (base/unword w)))])))))]
    (-> ctx
        (update :imports into imports)
        (update :definitions merge definitions')
        (update :env merge (ut/map-vals (fn [w] {:type :library
                                                 :library path
                                                 :fully-qualified w})
                                        mapping)))))

(defn handle-use [ctx specs]
  (reduce handle-use* ctx specs))

(defmulti pragma :id)

(defmethod pragma :default [{:keys [expr id]}]
  [(base/json-values-walk #(if (base/word? %) (base/unword %) %) expr) (jj/word id)])

(defn pragmaexpand [ctx x]
  (update ctx :body (fnil #(apply conj %1 %2) []) (pragma x)))

(defn analyze-expr [ctx expr]
  ;; (prn "---CTX" ctx)
  (case (:state ctx)
    :consume (condp = expr
               (jj/word "#import")
               (assoc ctx :state :import)

               (jj/word "#def")
               (assoc ctx :state :def)

               (jj/word "#declare")
               (assoc ctx :state :declare)

               (jj/word "#use")
               (assoc ctx :state :use)

               (if (pragma? expr)
                 (assoc ctx
                        :state :pragmaexpand
                        :pragma (subs expr 2))
                 (let [v (if (base/word? expr)
                           ;; TODO add dead code elimination
                           (resolve-word ctx expr)
                           expr)]
                   (update ctx :body (fnil conj []) v))))
    :import (let [_ (assert (map? expr))]
              (-> (handle-import ctx expr)
                  (assoc :state :consume)))
    :pragmaexpand (-> (pragmaexpand ctx {:id (:pragma ctx)
                                         :expr expr})
                      (dissoc :pragma)
                      (assoc :state :consume))
    :def (do (assert (jj/word expr))
             (assoc ctx
                    :def {:name expr}
                    :state :def-body))
    :def-body (cond
                (and (not (base/word? expr))
                     (string? expr))
                (assoc-in ctx [:def :doc] expr)

                (map? expr)
                (update-in ctx [:def :options] merge expr)

                (sequential? expr)
                (let [{:keys [name] :as def} (:def ctx)
                      ctx' (-> ctx
                               (dissoc :def)
                               (assoc :state :consume)
                               (assoc-in [:env name] {:type :def
                                                      :fully-qualified name}))
                      def' (assoc def
                                  ;; FIXME do not allow imports / defs inside body def
                                  :body (:body (analyze* ctx' expr)))]
                  (assoc-in ctx' [:definitions name] def')))

    :use (let [_ (assert (sequential? expr))]
           (-> (handle-use ctx expr)
               (assoc :state :consume)))

    :declare (let [_ (assert (base/word? expr))]
               (-> ctx
                   (assoc-in [:env expr] {:type :declare
                                          :fully-qualified expr})))))

(defn analyze* [ctx body]
  (reduce analyze-expr ctx body))

(defn analyze
  [body {:keys [fs]}]
  (let [{:keys [definitions body imports]} (analyze* {:fs fs
                                                      :libs-cache (atom {})
                                                      :state :consume
                                                      :env init-env
                                                      :imports #{}}
                                                     body)]
    {"imports" imports
     "vocabulary" (->> (for [[word {:keys [body]}] definitions]
                         [word body])
                       (into {}))
     "body" body}))

#_(defn read
    [body {:keys [fs]}]
    (binding [*parsing-state* (atom {:vocabulary {}})
              *fs* fs]
      (let [body (read-body body)]
        {"imports" (:imports @*parsing-state*)
         "vocabulary" (:vocabulary @*parsing-state*)
         "body" body})))

(def ^:dynamic *parsing-state*)
(def ^:dynamic *fs*)

#_(defn set-defs [defs]
    (assert (map? defs))
    (doseq [[n body] defs
            :let [_ (assert (sequential? body))]]
      (swap! *parsing-state* update :vocabulary assoc (jj/word n) body)))

#_(defn set-imports [specs]
    (assert (map? specs))
    (doseq [[spec word] specs
            :let [[_ alias fun arity] (re-find #"(.+?)\.(.+)/(\d+)" spec)]]
      (swap! *parsing-state* update :imports assoc word {"alias" alias
                                                         "function" fun
                                                         "arity" (edn/read-string arity)})))



(defn parse
  ([body] (parse body {}))
  ([body options] (analyze (read body) options)))

#_(defn run [program]
    (jj/run (jj/load-program (jsonify (reader program)))))

(comment
  (print (-> "#import {\"codecs/html-encode/2\" html-encode} #def dub [\"a-aa\" shuffle +] 2 dub"
             (read) (analyze {})))

  (print (-> "#import {\"codecs/html-encode/2\" html-encode} 1 html-encode"
             (read) (analyze {})))

  (print (-> "#refer [utils/incrementer [utils/incrementer as incr refer [inc]]]
              utils/incrementer/inc incr/inc inc"
             (read) (analyze {:fs (in-memory-fs {"utils/incrementer" "#def inc [1 +]"})})))

  (print (-> "#refer [[lib1 refer [inc]]]
              #def foo [#refer [[lib2 refer [inc]]] inc]
              inc"
             (read) (analyze {:fs (in-memory-fs {"lib1" "#def inc [1]"
                                                 "lib2" "#def inc [2]"})})))

  (print (-> "#template [foo bar]"
             (read) (analyze {})))
  )
