(ns catparty.clexer
  (:require [catparty.lexer :as lex]))

;; C lexical analyzer

(def c-keywords
  ["auto"
   "break"
   "case"
   "char"
   "const"
   "continue"
   "default"
   "do"
   "double"
   "else"
   "enum"
   "extern"
   "float"
   "for"
   "goto"
   "if"
   "int"
   "long"
   "register"
   "return"
   "short"
   "signed"
   "sizeof"
   "static"
   "struct"
   "switch"
   "typedef"
   "union"
   "unsigned"
   "void"
   "volatile"
   "while"
   ])

(def c-keyword-patterns
  (mapv (fn [kw] [(re-pattern (str "^" kw)) (keyword (str "kw_" kw))]) c-keywords))

(def c-all-patterns
  (concat c-keyword-patterns))

(defn create-from-lines
  "Create a C lexer from sequence of lines."
  [lines]
  (lex/create-from-lines lines c-all-patterns))

(defn create-from-string
  "Create a C lexer from input string."
  [s]
  (lex/create-from-string s c-all-patterns))
