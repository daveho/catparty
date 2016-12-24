(ns catparty.cparser
  (:require [catparty.node :as node]
            [catparty.parser :as p]
            [catparty.lexer :as l]
            [catparty.clexer :as cl]
            [catparty.prettyprint :as pp]
            [clojure.set :as set]))

;; Recursive descent parser for C.
;; Adapted from the v3 and v4 ANTLR C grammars:
;;    http://www.antlr3.org/grammar/1153358328744/C.g
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

;; Set of tokens that can begin a declarator suffix
(def declarator-suffix-start-tokens
  #{:lparen :lbracket})


(defn parse-type-qualifier-list [token-seq & [ctx]]
  (p/accept-matching :type_qualifier_list
                     (l/make-token-type-pred type-qualifiers)
                     token-seq))


(defn parse-pointer [token-seq & [ctx]]
  ; pointer -> ^ '*' type-qualifier-list
  ; pointer -> ^ '*' type-qualifier-list pointer
  (let [pr (p/do-production :pointer [(p/expect :op_star) parse-type-qualifier-list] token-seq)
        remaining (:tokens pr)]
    (if (l/next-token-is? remaining :op_star)
      ; continue recursively
      (p/continue-production pr [parse-pointer])
      ; done
      pr)))


(defn parse-opt-pointer [token-seq & [ctx]]
  (if (l/next-token-is? token-seq :op_star)
    (p/do-production :opt_pointer [parse-pointer] token-seq)
    (p/do-production :opt_pointer [] token-seq)))


(declare parse-declarator)


(defn parse-direct-declarator-base [token-seq & [ctx]]
  ; direct-declarator-base -> identifier
  ; direct-declarator-base -> '(' declarator ')'
  (if (l/next-token-is? token-seq :identifier)
    (p/do-production :direct_declarator_base [(p/expect :identifier)] token-seq)
    (p/do-production :direct_declarator_base [(p/expect :lparen)
                                              parse-declarator
                                              (p/expect :rparen)] token-seq)))


;; FIXME: productions should be (from ANTLR 3 grammar)
;; declarator_suffix
;;     :   '[' constant_expression ']'
;;     |   '[' ']'
;;     |   '(' parameter_type_list ')'
;;     |   '(' identifier_list ')'
;;     |   '(' ')'
;; 	;
;; This looks doable with 2 tokens of lookahead.
;; 
(defn parse-declarator-suffix [token-seq & [ctx]]
  (if (l/next-token-is? token-seq :lparen)
    (p/do-production :declarator_suffix [(p/expect :lparen) (p/expect :rparen)] token-seq)
    (p/do-production :declarator_suffix [(p/expect :lbracket) (p/expect :rbracket)] token-seq)))


(defn parse-declarator-suffix-list [token-seq & [ctx]]
  ; Start by parsing one declarator suffix.
  (let [pr (p/do-production :declarator_suffix_list [parse-declarator-suffix] token-seq)
        remaining (:tokens pr)]
    (if (l/next-token-in? remaining declarator-suffix-start-tokens)
      ; Continue recursively.
      (p/continue-production pr [parse-declarator-suffix-list])
      ; Done.
      pr)))


(defn parse-opt-declarator-suffix-list [token-seq & [ctx]]
  (if (l/next-token-in? token-seq declarator-suffix-start-tokens)
    (p/do-production :opt_declarator_suffix_list [parse-declarator-suffix-list] token-seq)
    (p/do-production :opt_declarator_suffix_list [] token-seq)))


(defn parse-direct-declarator [token-seq & [ctx]]
  (p/do-production :direct_declarator [parse-direct-declarator-base
                                     parse-opt-declarator-suffix-list] token-seq))


(defn parse-declarator [token-seq & [ctx]]
  (p/do-production :declarator [parse-opt-pointer parse-direct-declarator] token-seq))


;; TODO: this is just a placeholder for now
(defn parse-initializer [token-seq & [ctx]]
  (p/do-production :initializer [(p/expect :dec_literal)] token-seq ctx))


;; TODO: this is just a placeholder for now
(defn parse-compound-statement [token-seq & [ctx]]
  (p/do-production :compound_statement [(p/expect :lbrace) (p/expect :rbrace)] token-seq ctx))


;; Ok, here is a complicated part of the parser.
;; We want to allow function definitions to be parsed here if
;; (in the parsing context) allow_func is true.
;; A function definition is very much like an init declarator, except:
;;
;;    - there is no '=' (i.e., no initialization)
;;    - there is a compound statement immediately after the declarator
;;
;; parse-init-declarator-list will also need to be aware of
;; this issue, so that it avoids attempting to parse further
;; declarators in the same declaration.  Also, parse-declaration
;; will need to check to see if a function definition was produced,
;; in which case it should not require a terminating ';' token.
;;
(defn parse-init-declarator [token-seq & [ctx]]
  (let [pr (p/do-production :init_declarator [parse-declarator] token-seq)
        remaining (:tokens pr)]
    (cond
     ; If the next token is the assignment operator,
     ; continue the production to parse the initializer.
     (l/next-token-is? remaining :op_assign)
     (p/continue-production pr [(p/expect :op_assign) parse-initializer] ctx)

     ; If a function definition is allowed, and the next token
     ; is an open brace, then convert the result into a function definition,
     ; and continue the production by parsing a compound statement.
     (and (:allow_func ctx) (l/next-token-is? remaining :lbrace))
     (let [pr2 (p/relabel-parse-result pr :function_definition)]
       (p/continue-production pr2 [parse-compound-statement] ctx))

     ; Otherwise, end the production here (there is no initializer,
     ; and this is not a function definition)
     :else pr)))


(defn parse-declaration-specifiers [token-seq & [ctx]]
  (p/accept-matching :declaration_specifiers
                     (l/make-token-type-pred declaration-specifiers)
                     token-seq))


(defn parse-init-declarator-list [token-seq & [ctx]]
  ; Initial state:
  ;   init-declarator-list -> ^ init-declarator
  ;   init-declarator-list -> ^ init-declarator ',' init-declarator-list
  ; Start by parsing just an init declarator.
  (let [pr (p/do-production :init_declarator_list [parse-init-declarator] token-seq ctx)
        remaining (:tokens pr)]
    ; Grab the init declarator we just parsed, in case it turns
    ; out to be a function definition.
    (let [first-child (node/get-child (:node pr) 0)]
      (cond
       ; If a function definition was parsed, end the production here
       ; (a function definition can have only one declarator).
       (= (:symbol first-child) :function_definition) pr

       ; If the next token is a comma, continue recursively, and
       ; update the parsing context to indicate that function definitions
       ; will not be allowed.  (See above :-)
       (l/next-token-is? remaining :comma)
       (p/continue-production pr [(p/expect :comma) parse-init-declarator-list] (dissoc ctx :allow_func))

       ; No more declarators, so end the production here
       :else pr))))


(defn parse-opt-init-declarator-list [token-seq & [ctx]]
  ; do we see a declarator?
  (if (l/next-token-in? token-seq declarator-start-tokens)
    ; there is at least one declarator
    (p/do-production :opt_init_declarator_list [parse-init-declarator-list] token-seq ctx)
    ; no declarators
    (p/do-production :opt_init_declarator_list [] token-seq)))


; Check whether given node is a function definition.
(defn is-function-definition? [node]
  (= (:symbol node) :function_definition))


(defn parse-declaration [token-seq & [ctx]]
  ; Parse just the declaration specifiers and (optional) init declarator list.
  ; We will need to know whether a function definition was parsed.
  (let [pr (p/do-production :declaration [parse-declaration-specifiers
                                          parse-opt-init-declarator-list] token-seq ctx)]
    ; Check the second child's (opt init declarator list)
    ; first child's (init declarator list)
    ; first child (the first init declarator in the init declarator list)
    ; to see if it's a function definition.
    (if (node/check-path (:node pr) [1 0 0] is-function-definition?)
      ; A function definition was parsed!
      ; The declaration ends here.
      pr
      ; One or more non-function-definition init declarators
      ; were parsed.  Require a semicolon to terminate the
      ; overall declaration.
      (p/continue-production pr [(p/expect :semicolon)]))))


(defn parse-declaration-list [token-seq & [ctx]]
  ; Initial state is:
  ;   declaration-list -> ^ declaration
  ;   declaration-list -> ^ declaration declaration-list
  ; Start by parsing just a declaration.
  (let [pr (p/do-production :declaration_list [parse-declaration] token-seq ctx)
        remaining (:tokens pr)]
    ; See if declaration list continues.
    (if (empty? remaining)
      ; No more tokens, so end declaration list.
      pr
      ; Declaration list continues.
      (p/continue-production pr [parse-declaration-list] ctx))))


(defn parse [token-seq]
  (:node (parse-declaration-list token-seq {:allow_func true})))


;; Just for testing...

;; (def testprog
;; "int x;
;; char *p;
;; double *q[];")

(def testprog
"int f() {
}")

(def t (parse (l/token-sequence (cl/create-from-string testprog))))
