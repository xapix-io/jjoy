(ns jjoy.human
  (:require [clojure.edn :as edn]
            [jjoy.core :as jj]
            [instaparse.core :as insta]))

(def grammar "
  BODY = EXPR*
  <EXPR> = WHITESPACE* (NUMBER|STRING|ARRAY|OBJECT|NULL|TRUE|FALSE|WORD|PRAGMA) WHITESPACE*
  PRAGMA = <'#'> WORD EXPR
  WORD = #'[-+]' | #'[^-\\s\\[\\]{};\"\\d+:,#][^\\s\\[\\]{};\":,]*' | ESCAPED_WORD
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

(def reader
  (insta/parser grammar))

(declare jsonify-expr jsonify-body)

(defn jsonify-keypair [[_ [k-type k] v]]
  [(case k-type
     :WORD k
     :STRING (edn/read-string k))
   (jsonify-expr v)])

(def ^:dynamic *parsing-state*)

(defn set-defs [defs]
  (assert (map? defs))
  (doseq [[n body] defs
          :let [_ (assert (sequential? body))]]
    (swap! *parsing-state* update :vocabulary assoc (jj/word n) body)))

(defn set-imports [specs]
  (assert (map? specs))
  (doseq [[spec word] specs
          :let [[_ alias fun arity] (re-find #"(.+?)\.(.+)/(\d+)" spec)]]
    (swap! *parsing-state* update :imports assoc word {"alias" alias
                                                      "function" fun
                                                      "arity" (edn/read-string arity)})))

(defn jsonify-expr [[type & xs]]
  (case type
    :TRUE true
    :FALSE false
    :NULL nil
    (:NUMBER :STRING) (edn/read-string (first xs))
    :WORD (jj/word (first xs))
    :OBJECT (into {} (map jsonify-keypair xs))
    :ARRAY (let [[body] xs] (jsonify-body body))
    :PRAGMA (let [[[_ word] e] xs]
              (let [e' (jsonify-expr e)]
                (case word
                  "word" (do (assert (string? e'))
                             (jj/word e'))
                  "defs" (do (set-defs e')
                             ::ignore)
                  "import" (do (set-imports e')
                               ::ignore)
                  "." ::ignore)))))

(defn jsonify-body [[_ & exprs]]
  (->> exprs
       (map jsonify-expr)
       (filter #(not= ::ignore %))
       (vec)))

(defn jsonify
  [body]
  (binding [*parsing-state* (atom {:vocabulary {}})]
    (let [body (jsonify-body body)]
      {"imports" (:imports @*parsing-state*)
       "vocabulary" (:vocabulary @*parsing-state*)
       "body" body})))

(defn parse [body] (jsonify (reader body)))

(defn run [program]
  (jj/run (jj/load-program (jsonify (reader program)))))

(comment
  (insta/parses parser "# ad
" :start :ONE_LINE_COMMENT)
  (insta/parses parser "#import [foo/2 [bar/3 as foo]] from lala")
  (insta/parses parser "#. \"comment\"")
  (insta/parses parser "foo = dip i;\n\n bar = dar b; foo.bar")
  (insta/parses reader "=")
  (-> (jsonify (reader (slurp "/Users/prepor/Dropbox/notes/demo/24.09.2019/users.jjoy")))
      (clojure.pprint/pprint))
  (jsonify (reader "arr(1).foo"))
  (parse "import []")

  (parser "1")
  (parser "\"foo\\\\ \"")
  (run '{:vocabulary {dub (dup +)}
         :body (3 dub)})

  (parser " 2 +")
  )
