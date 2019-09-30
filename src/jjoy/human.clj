(ns jjoy.human
  (:require [clojure.edn :as edn]
            [jjoy.core :as jj]
            [instaparse.core :as insta]))

(def grammar "
  PROGRAM = FFC? DEFINITIONS BODY
  FFC = WHITESPACE* <'import'> WHITESPACE+ BRACKET_OPEN WHITESPACE* (FFC_ITEM WHITESPACE+)* FFC_ITEM WHITESPACE* BRACKET_CLOSE WHITESPACE+<'from'> WHITESPACE+ WORD WHITESPACE+
  FFC_ITEM = WORD_WITH_ARITY | BRACKET_OPEN WHITESPACE*  WORD_WITH_ARITY WHITESPACE+ <'as'> WHITESPACE+ WORD WHITESPACE* BRACKET_CLOSE
  WORD_WITH_ARITY = WORD <'/'> NUMBER
  DEFINITIONS = DEFINITION*
  BODY = WHITESPACE* (EXPR WHITESPACE+)* EXPR WHITESPACE*
  DEFINITION = WHITESPACE* WORD WHITESPACE+ <'='> WHITESPACE+ BODY <';'>
  <EXPR> = NUMBER|STRING|ARRAY|OBJECT|NULL|TRUE|FALSE|WORD
  WORD = #'[-+]' | #'[^-\\s\\[\\]{};\"\\d+:,][^\\s\\[\\]{};\":,]*'
  ARRAY = BRACKET_OPEN BODY BRACKET_CLOSE
  <BRACKET_OPEN> = <'['>
  <BRACKET_CLOSE> = <']'>
  <WHITESPACE> = <#'[\\s\\n:,]+'>
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
    :ARRAY (let [[[_ & xs]] xs] (mapv jsonify-expr xs))))

(defn jsonify-body [[_ & exprs]]
  (mapv jsonify-expr exprs))

(defn jsonify-definition [[_ [_ n] body]]
  [(jj/word n) (jsonify-body body)])

(defn jsonify [[_ [_ & definitions] body]]
  {"vocabulary" (into {} (map jsonify-definition definitions))
   "body" (jsonify-body body)})

(defn parse [body] (jsonify (parser body)))

(defn run [program]
  (jj/run (jj/load-program (jsonify (parser program)))))

(comment
  (insta/parses parser "foo/2" :start :WORD_WITH_ARITY)
  (insta/parses parser "foo = dip i;\n\n bar = dar b; foo.bar")
  (insta/parses parser "-")
  (jsonify (parser "[1 2 3]"))
  (parse "import []")

  (parser "import [foo/2 bar/2] from clojud")
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
