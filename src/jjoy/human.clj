(ns jjoy.human
  (:require [clojure.edn :as edn]
            [jjoy.core :as jj]
            [instaparse.core :as insta]))

(def grammar "
  PROGRAM = DEFINITIONS BODY
  DEFINITIONS = DEFINITION*
  BODY = (EXPR WHITESPACE+)* EXPR WHITESPACE*
  DEFINITION = WORD WHITESPACE+ <'='> WHITESPACE+ BODY WHITESPACE* <';'> WHITESPACE*
  <EXPR> = NUMBER|STRING|ARRAY|OBJECT|NULL|TRUE|FALSE|WORD
  WORD = #'[-+]' | #'[^-\\s\\[\\]{};\"\\d+:,][^\\s\\[\\]{};\":,]*'
  ARRAY = BRACKET_OPEN WHITESPACE* (EXPR WHITESPACE+)* EXPR WHITESPACE* BRACKET_CLOSE
  <BRACKET_OPEN> = <'['>
  <BRACKET_CLOSE> = <']'>
  <WHITESPACE> = <#'[\\s\\n:,]+'>
  KEY_VALUE_PAIR = (STRING | WORD) WHITESPACE* EXPR
  OBJECT = CURLY_OPEN WHITESPACE* KEY_VALUE_PAIR WHITESPACE* (WHITESPACE* KEY_VALUE_PAIR WHITESPACE*)* CURLY_CLOSE
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

(def parser
  (insta/parser grammar))

(declare jsonify-expr)

(defn jsonify-keypair [[_ [k-type k] v]]
  [(case k-type
     :WORD k
     :STRING (edn/read-string k))
   (jsonify-expr v)])

(defn jsonify-expr [[type & xs]]
  (case type
    :TRUE true
    :FALSE false
    :NULL nil
    (:NUMBER :STRING) (edn/read-string (first xs))
    :WORD (jj/word (first xs))
    :OBJECT (into {} (map jsonify-keypair xs))
    :ARRAY (mapv jsonify-expr xs)))

(defn jsonify-body [[_ & exprs]]
  (mapv jsonify-expr exprs))

(defn jsonify-definition [[_ [_ n] body]]
  [(jj/word n) (let [body' (jsonify-body body)]
                   (fn [stack program] [stack (concat body' program)]))])

(defn jsonify [[_ [_ & definitions] body]]
  {:vocabulary (into {} (map jsonify-definition definitions))
   :body (jsonify-body body)})

(defn run [program]
  (let [{:keys [vocabulary body]} (jsonify (parser program))]
    (jj/run vocabulary body)))

(comment
  (insta/parses parser "foo = dip i;" :start :DEFINITION)
  (insta/parses parser "foo = dip i;\n\n bar = dar b; foo.bar")
  (insta/parses parser "-")
  (jsonify (parser "[1 2 3]"))
  (run "dub = dup +; 3 dub")

  (parser "[1 2 3]")
  (parser "\"foo\\\\ \"")
  (run '{:vocabulary {dub (dup +)}
         :body (3 dub)})

  (parser "[ \"format\"]")

  (parser "{ url [ \"format\" \"http://samples-dev.ix-io.net/api/users/~d\" \".\"]
               method \"get\" }")

  (parser "get-vehicles \"data.user-id\" query [get-user \"data.first-name\" query] map {status 200
   body {user-names [\".\"]}} template
")

  (jsonify (parser "get-vehicles = { url \"http://samples-dev.ix-io.net/api/vehicles\",
                   method \"get\" }
                   http-request \"body\" query;
  get-user = { url [ \"format\" \"http://samples-dev.ix-io.net/api/users/~d\" \".\"]
               method \"get\" } template
             http-request \"body\" query;

get-vehicles
  \"data.user-id\"
query
  [get-user \"data.first-name\" query]
map
  {status 200
   body {user-names [\".\"]}}
template
"))
  )
