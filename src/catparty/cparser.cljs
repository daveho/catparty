(ns catparty.cparser
  (:require [catparty.parser :as parser]
            [catparty.lexer :as lexer]
            [catparty.clexer :as clexer]
            [catparty.prettyprint :as pp])
  )

;; Recursive descent parser for C.
;; Adapted from the ANTLR C grammar:
;;    https://github.com/antlr/grammars-v4/blob/master/c/C.g4

(def storage-class-specifiers
  #{:kw_typedef :kw_extern :kw_static :kw_auto :kw_register})

(def type-specifiers
  #{:kw_void :kw_char :kw_short :kw_int :kw_long :kw_float :kw_double :kw_signed :kw_unsigned})

;; FIXME: just handle identifiers for now
(defn parse-declarator [token-seq]
  (parser/do-production :declarator [(parser/expect :identifier)] token-seq))

;; FIXME: allow initialization
(defn parse-init-declarator [token-seq]
  (parser/do-production :init_declarator [parse-declarator] token-seq))

;; FIXME: symbol table lookup here needed to detect typedef names.
(defn is-declaration-specifier? [token]
  (let [token-type (lexer/get-token-type token)]
    (or (contains? storage-class-specifiers token-type)
        (contains? type-specifiers token-type))))

(defn parse-declaration-specifiers [token-seq]
  (parser/accept-matching :declaration_specifiers is-declaration-specifier? token-seq))

(defn parse-init-declarator-list [token-seq]
  ; init-declarator-list -> ^ init-declarator
  ; init-declarator-list -> ^ init-declarator ',' init-declarator-list
  (let [startpr (parser/initial-parse-result token-seq)
        ; Parse one init declarator
        pr (parser/apply-production startpr [parse-init-declarator])
        remaining-tokens (:tokens pr)]
    ; init-declarator-list -> init-declarator ^
    ; init-declarator-list -> init-declarator ^ ',' init-declarator-list
    (if (lexer/next-token-is? remaining-tokens :comma)
      ; init declarator list continues...
      (parser/complete-production :init_declarator_list
                                  (parser/apply-production pr [(parser/expect :comma) parse-init-declarator-list]))
      ; no more init declarators
      (parser/complete-production :init_declarator_list pr))))

(defn parse-opt-init-declarator-list [token-seq]
  ; declarators always begin with either an identifier or left paren
  (if (lexer/next-token-in? token-seq #{:identifier :lparen})
    ; there is at least one declarator
    (parser/do-production :opt_init_declarator_list [parse-init-declarator-list] token-seq)
    ; no declarators
    (parser/do-production :opt_init_declarator_list [] token-seq)))

(defn parse-declaration [token-seq]
  (parser/do-production :declaration [parse-declaration-specifiers
                                      parse-opt-init-declarator-list
                                      (parser/expect :semicolon)] token-seq))

(defn parse-declaration-list [token-seq]
  ; Possibilities are:
  ;    declaration-list -> ^ declaration
  ;    declaration-list -> ^ declaration declaration-list
  (let [startpr (parser/initial-parse-result token-seq)
        ; Parse a single external declaration
        pr (parser/apply-production startpr [parse-declaration])
        remaining-tokens (:tokens pr)]
    (if (empty? remaining-tokens)
      ; At EOF, end the production here
      (parser/complete-production :declaration-list pr)
      ; Not at EOF, continue recursively
      (parser/complete-production :declaration-list
                                  (parser/apply-production pr [parse-declaration-list]))
      )
  ))


(defn parse [token-seq]
  (:node (parse-declaration-list token-seq)))

;; Just for testing...

(def testprog "int x;")
(def t (parse (lexer/token-sequence (clexer/create-from-string testprog))))


