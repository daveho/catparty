(ns catparty.cparser
  (:require [catparty.parser :as p]
            [catparty.lexer :as l]
            [catparty.clexer :as cl]
            [catparty.prettyprint :as pp]
            [clojure.set :as set]))

;; Recursive descent parser for C.
;; Adapted from the ANTLR C grammar:
;;    https://github.com/antlr/grammars-v4/blob/master/c/C.g4

(def storage-class-specifiers
  #{:kw_typedef :kw_extern :kw_static :kw_auto :kw_register})

(def type-specifiers
  #{:kw_void :kw_char :kw_short :kw_int :kw_long :kw_float :kw_double :kw_signed :kw_unsigned})

(def declaration-specifiers (set/union storage-class-specifiers type-specifiers))

(def type-qualifiers
  #{:kw_const :kw_restrict :kw_volatile})

;; Set of tokens that can begin a declarator
(def declarator-start-tokens
  #{:identifier :lparen :op_star})


(defn parse-type-qualifier-list [token-seq]
  (p/accept-matching :type_qualifier_list
                     (l/make-token-type-pred type-qualifiers)
                     token-seq))


(defn parse-pointer [token-seq]
  ; pointer -> ^ '*' type-qualifier-list
  ; pointer -> ^ '*' type-qualifier-list pointer
  (let [pr (p/do-production :pointer [(p/expect :op_star) parse-type-qualifier-list] token-seq)
        remaining (:tokens pr)]
    (if (l/next-token-is? remaining :op_star)
      ; continue recursively
      (p/continue-production pr [parse-pointer])
      ; done
      pr)))


(defn parse-opt-pointer [token-seq]
  (if (l/next-token-is? token-seq :op_star)
    (p/do-production :opt_pointer [parse-pointer] token-seq)
    (p/do-production :opt_pointer [] token-seq)))


;; FIXME: just handle identifiers for now
(defn parse-direct-declarator [token-seq]
  (p/do-production :direct_declarator [(p/expect :identifier)] token-seq))


(defn parse-declarator [token-seq]
  (p/do-production :declarator [parse-opt-pointer parse-direct-declarator] token-seq))


;; FIXME: allow initialization
(defn parse-init-declarator [token-seq]
  (p/do-production :init_declarator [parse-declarator] token-seq))


(defn parse-declaration-specifiers [token-seq]
  (p/accept-matching :declaration_specifiers
                     (l/make-token-type-pred declaration-specifiers)
                     token-seq))


(defn parse-init-declarator-list [token-seq]
  ; Initial state:
  ;   init-declarator-list -> ^ init-declarator
  ;   init-declarator-list -> ^ init-declarator ',' init-declarator-list
  ; Start by parsing just an init declarator.
  (let [pr (p/do-production :init_declarator_list [parse-init-declarator] token-seq)
        remaining (:tokens pr)]
    ; If the next token is a comma, continue recursively.
    (if (l/next-token-is? remaining :comma)
      ; init-declarator-list -> init-declarator ^ ',' init-declarator-list
      (p/continue-production pr [(p/expect :comma) parse-init-declarator-list])
      ; init-declarator-list -> init-declarator ^
      pr)))


(defn parse-opt-init-declarator-list [token-seq]
  ; do we see a declarator?
  (if (l/next-token-in? token-seq declarator-start-tokens)
    ; there is at least one declarator
    (p/do-production :opt_init_declarator_list [parse-init-declarator-list] token-seq)
    ; no declarators
    (p/do-production :opt_init_declarator_list [] token-seq)))


(defn parse-declaration [token-seq]
  (p/do-production :declaration [parse-declaration-specifiers
                                 parse-opt-init-declarator-list
                                 (p/expect :semicolon)] token-seq))


(defn parse-declaration-list [token-seq]
  ; Initial state is:
  ;   declaration-list -> ^ declaration
  ;   declaration-list -> ^ declaration declaration-list
  ; Start by parsing just a declaration.
  (let [pr (p/do-production :declaration_list [parse-declaration] token-seq)
        remaining (:tokens pr)]
    ; See if declaration list continues.
    (if (empty? remaining)
      ; No more tokens, so end declaration list.
      pr
      ; Declaration list continues.
      (p/continue-production pr [parse-declaration-list]))))


(defn parse [token-seq]
  (:node (parse-declaration-list token-seq)))


;; Just for testing...

(def testprog
"int x;
char *p;")
(def t (parse (l/token-sequence (cl/create-from-string testprog))))
