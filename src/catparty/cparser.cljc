; -*- mode: clojure -*-

(ns catparty.cparser
  (:require [catparty.node :as node]
            [catparty.parser :as p]
            [catparty.lexer :as l]
            [catparty.clexer :as cl]
            [catparty.prettyprint :as pp]
            [catparty.exc :as exc]
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

;; Set of tokens that are literal values
;; TODO: others
(def literals
  #{:fp_literal :dec_literal :hex_literal :char_literal :string_literal})

;; Set of assignment operator tokens
(def assignment-operators
  #{:op_assign :op_lshift_assign :op_rshift_assign :op_bit_or_assign :op_bit_and_assign
    :op_bit_xor_assign :op_plus_assign :op_minus_assign :op_mul_assign :op_div_assign
    :op_mod_assign})

;; Binary operator precedences:
;; note that comma, assignment, and conditionals (?:) are
;; not parsed by precedence climbing because the conditional
;; operator isn't a binary operator.  These are easy
;; enough to handle by recursive descent, however, and
;; fortunately for us the assignment operators are
;; right-associative.
(def binop-precedence
  {:op_or 0,
   :op_and 1,
   :op_bit_or 2,
   :op_bit_xor 3,
   :op_bit_and 4,
   :op_eq 5,
   :op_ne 5,
   :op_lt 6,
   :op_gt 6,
   :op_lte 6,
   :op_gte 6,
   :op_lshift 7,
   :op_rshift 7,
   :op_plus 8,
   :op_minus 8,
   :op_star 9,
   :op_div 9,
   :op_mod 9,
   ; All higher-precedence constructs (casts, unary operators,
   ; etc.) are handled by parse-cast-expression
   })

;; Binary operator associativities:
;; all of the binary operators we handle by precedence climbing are
;; left asssociative
(def binop-associativity
  (into {} (map (fn [k] [k :left]) (keys binop-precedence))))


(def is-literal?
  (fn [t]
    (let [pred (l/make-token-type-pred literals)
          result (pred t)
          lexeme (first t)]
      ;(println lexeme (if result "is" "is not") "a literal")
      result)))


(defn parse-literal [token-seq & [ctx]]
  ;(println "Parsing literal!")
  (if (not (l/next-token-matches? token-seq is-literal?))
    (exc/throw-exception "Expected literal")
    (p/do-production :literal [(p/expect (l/next-token-type token-seq))] token-seq ctx)))


;; TODO: just a place holder for now (allowing only integer literals)
(defn parse-cast-expression [token-seq & [ctx]]
  (let [pr (p/do-production :cast_expression [parse-literal] token-seq ctx)]
    (do
      ;(println "After cast expression: " (:tokens pr))
      pr)
    ))


(def c-infix-operators
  (p/make-operators binop-precedence binop-associativity parse-cast-expression))


(defn parse-logical-or-expression [token-seq & [ctx]]
  (let [ctx2 (assoc ctx :operators c-infix-operators)]
    (let [pr (p/parse-infix-expression token-seq ctx2)]
      (do
        ;(println "After infix parsing: " (:tokens pr))
        pr
        )
      )))


(declare parse-expression)


(defn parse-conditional-expression [token-seq & [ctx]]
  ; State is:
  ;   conditional-expression -> ^ logical-or-expression
  ;   conditional-expression -> ^ logical-or-expression '?' expression : conditional-expression
  ; Start by parsing a logical or expression.
  (let [pr (p/do-production :conditional_expression [parse-logical-or-expression] token-seq ctx)
        remaining (:tokens pr)]
    ; See if the expression just parsed is a conditional expression
    ; by looking ahead to see if there's a '?'
    (if (l/next-token-is? remaining :ques)
      ; Parse clauses of conditional operator
      (p/continue-production pr [(p/expect :ques)
                               parse-expression
                               (p/expect :colon)
                               parse-conditional-expression] ctx)
      ; Production ends here
      pr)))


(defn parse-assignment-expression [token-seq & [ctx]]
  ; State is:
  ;    assignment-expression -> ^ conditional-expression
  ;    assignment-expression -> ^ conditional-expression assignment-operator assignment-expression
  ; Note that in K&R the beginning of the right hand side of the
  ; second production is actually "unary-expression", which would
  ; exclude some kinds of expressions that are never lvalues.
  ; We'll just deal with that during semantic analysis.
  ; Start by parsing a conditional expression.
  (let [pr (p/do-production :assignment_expression [parse-conditional-expression] token-seq ctx)
        remaining (:tokens pr)]
    ; See if there is an assignment operator
    (if (l/next-token-in? remaining assignment-operators)
      ; Continue production recursively.
      (p/continue-production pr [(p/expect (l/next-token-type remaining))
                                 parse-assignment-expression] token-seq ctx)
      ; Production ends here
      (do
        ;(println "After parsing assignment expression: " (:tokens pr))
        pr))))


(defn parse-expression [token-seq & [ctx]]
  ; State is:
  ;   expression -> ^ assignment-expression
  ;   expression -> ^ assignment-expression ',' expression
  ; NOTE: this will parse the comma operator as right associative,
  ; so we'll have to fix the resulting parse tree.
  ; TODO: implement the restructuring of the parse tree.
  ; TODO: there might be a way to do this by not using p/continue-production,
  ; but rather using ad-hoc code to parse a comma-separated sequence
  ; of assignment expressions and then transform the result into the
  ; correct (left associative) parse tree.
  (let [pr (p/do-production :expression [parse-assignment-expression] token-seq ctx)
        remaining (:tokens pr)]
    (if (l/next-token-is? remaining :comma)
      ; Production continues recursively
      (p/continue-production pr [(p/expect :comma) parse-expression] ctx)
      ; Production ends here
      (do
        ;(println "Tokens after parsing expression: " (:tokens pr))
        pr)
      )))


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


(defn parse-initializer [token-seq & [ctx]]
  (p/do-production :initializer [parse-assignment-expression] token-seq ctx))


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
    (if (node/check-node (:node pr) [1 0 0] is-function-definition?)
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
"
int x;
char *p;
double *q[];
int a = 2 + 3;
int f()
{
}
int b = 42 + 1 << 5;
")


(def token-seq (l/token-sequence (cl/create-from-string testprog)))
(def t (parse token-seq))
