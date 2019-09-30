(ns jjoy.human
  (:require [clojure.edn :as edn]
            [jjoy.core :as jj]
            [instaparse.core :as insta]))

(def grammar "
  PROGRAM = PRAGMAS ((DEFINITIONS+ (<'###'> BODY)?) | BODY)
  PRAGMAS = PRAGMA*
  <PRAGMA> = IMPORT | USE
  IMPORT = WHITESPACE* <'#import'> WHITESPACE+ BRACKET_OPEN WHITESPACE* (FFC_ITEM WHITESPACE*)+ BRACKET_CLOSE WHITESPACE+ <'from'> WHITESPACE+ WORD
  USE = WHITESPACE* <'#use'> WHITESPACE+ BRACKET_OPEN WHITESPACE* (USE_ITEM WHITESPACE*)+ BRACKET_CLOSE

  FFC_ITEM = WORD | FFC_ALIAS
  FFC_ALIAS = BRACKET_OPEN WHITESPACE* WORD WHITESPACE+ <'as'> WHITESPACE+ WORD WHITESPACE* BRACKET_CLOSE
  USE_ITEM = WORD USE_AS? USE_REFER?
  USE_AS = WHITESPACE+ <'as'> WHITESPACE+ WORD
  USE_REFER = WHITESPACE+ <'refer'> WHITESPACE+ BRACKET_OPEN WHITESPACE* (WORD WHITESPACE*)+ BRACKET_CLOSE

  DEFINITIONS = DEFINITION*
  DEFINITION = WHITESPACE* <'#def'> WHITESPACE+ WORD WHITESPACE+ BODY
  BODY = EXPR*
  <EXPR> = WHITESPACE* (NUMBER|STRING|ARRAY|OBJECT|NULL|TRUE|FALSE|WORD|<COMMENT>) WHITESPACE*
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

(defn parse-pragma [[t & args]]
  (case t
    :IMPORT ()
    :USE))

(defn jsonify
  [input]
  (let [[pragmas definitions body]
        (case (count input)
          4 (let [[_ pragmas [_ & definitions] body] input]
              [pragmas definitions body])
          3 (let [[_ pragmas body] input]
              [pragmas [] body]))]
    {"vocabulary" (into {} (map jsonify-definition definitions))
     "body" (jsonify-body body)}))

(defn parse [body] (jsonify (parser body)))

(defn run [program]
  (jj/run (jj/load-program (jsonify (parser program)))))

(comment
  (insta/parses parser "# ad
" :start :ONE_LINE_COMMENT)
  (insta/parses parser "#import [foo/2 [bar/3 as foo]] from lala")
  (insta/parses parser "#def dup 1 +")
  (insta/parses parser "foo = dip i;\n\n bar = dar b; foo.bar")
  (insta/parses parser "-")
  (jsonify (parser "1"))
  (parse "import []")

  (parser "1")
  (parser "\"foo\\\\ \"")
  (run '{:vocabulary {dub (dup +)}
         :body (3 dub)})

  (parser " 2 +")
  )
