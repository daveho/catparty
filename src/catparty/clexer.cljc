; -*- mode: clojure -*-

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
   "double"
   "do"
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

(def c-identifier-pattern
  [[#"^[A-Za-z_][A-Za-z0-9_]*" :identifier]])

(def c-operator-patterns
  [[#"^==" :op_eq]
   [#"^=" :op_assign]
   [#"^<<=" :op_lshift_assign]
   [#"^<<" :op_lshift]
   [#"^<=" :op_lte]
   [#"^<" :op_lt]
   [#"^>>=" :op_rshift_assign]
   [#"^>>" :op_rshift]
   [#"^>=" :op_gte]
   [#"^>" :op_gt]
   [#"^!=" :op_ne]
   [#"^!" :op_not]
   [#"^\|\|" :op_or]
   [#"^\|=" :op_bit_or_assign]
   [#"^\|" :op_bit_or]
   [#"^&&" :op_and]
   [#"^&=" :op_bit_and_assign]
   [#"^&" :op_bit_and]
   [#"^\^=" :op_bit_xor_assign]
   [#"^\^" :op_bit_xor]
   [#"^~" :op_bit_compl]
   [#"^\+=" :op_plus_assign]
   [#"^\+" :op_plus]
   [#"^-=" :op_minus_assign]
   [#"^-" :op_minus]
   [#"^\*=" :op_mul_assign]
   [#"^\*" :op_star] ; note that this means multiplication and pointer/dereference
   [#"^/=" :op_div_assign]
   [#"^/" :op_div]
   [#"^%=" :op_mod_assign]
   [#"^%" :op_mod]
   ]
  )

(def c-punct-patterns
  [[#"^\(" :lparen]
   [#"^\)" :rparen]
   [#"^\[" :lbracket]
   [#"^\]" :rbracket]
   [#"^\{" :lbrace]
   [#"^\}" :rbrace]
   [#"^:" :colon]
   [#"^;" :semicolon]
   [#"^," :comma]
   [#"^\?" :ques]
   ])

;; TODO: other kinds of literals
(def c-literal-patterns
  [[#"^[0-9]+\.[0-9]*([Ee](\+|-)?[0-9+])?[fFlL]?" :fp_literal]
   [#"^\.[0-9]+([Ee](\+|-)?[0-9+])?[fFlL]?" :fp_literal]
   [#"^[0-9]+" :dec_literal]
   [#"^0[Xx][0-9A-Fa-z]+(UuLl)*" :hex_literal]
   [#"^'(\\([ntvbrfa\\?'\"]|[0-7][0-7][0-7]|[0-9A-Fa-f][0-9A-Fa-f])|[^\\'])'" :char_literal]
   [#"^\"(\.|[^\"])\"" :string_literal]])

(def c-all-patterns
  (concat c-keyword-patterns
          c-identifier-pattern
          c-operator-patterns
          c-punct-patterns
          c-literal-patterns))

(defn create-from-lines
  "Create a C lexer from sequence of lines."
  [lines]
  (lex/create-from-lines lines c-all-patterns))

(defn create-from-string
  "Create a C lexer from input string."
  [s]
  (lex/create-from-string s c-all-patterns))
