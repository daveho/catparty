; -*- mode: clojure -*-

;; Cat Party - C parser in Clojure/ClojureScript
;; Copyright (c) 2016-2017, David H. Hovemeyer <david.hovemeyer@gmail.com>
;;
;; This is free software distributed under the GNU Public License, version 3,
;; or any later version.  See COPYING.txt for details.

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
;; Also useful, ISO C grammar with opt factored out (very helpful
;; for figuring out how to handle abstract vs. concrete
;; declarators):
;;    http://www.cs.dartmouth.edu/~mckeeman/cs48/references/c.html


;; ----------------------------------------------------------------------
;; Data
;; ----------------------------------------------------------------------

(def storage-class-specifier-tokens
  #{:kw_typedef :kw_extern :kw_static :kw_auto :kw_register})

; Single-token type specifiers: excludes typedef names and
; struct/union types.
(def type-specifier-tokens
  #{:kw_void :kw_char :kw_short :kw_int :kw_long :kw_float :kw_double :kw_signed :kw_unsigned})

(def type-qualifier-tokens
  #{:kw_const :kw_restrict :kw_volatile})

;; Tokens that can start a type specifier.
;; FIXME: does not handle typedef names
(def type-specifier-start-tokens
  (set/union type-specifier-tokens #{:kw_struct :kw_union}))

;; Tokens that can start a type specifier or qualifier.
;; FIXME: does not handle typedef names
(def type-specifier-or-qualifier-start-tokens
  (set/union type-qualifier-tokens type-specifier-start-tokens))

;; Single token declaration specifiers.
;; Note that these do not include typedef names and
;; struct/union types.
;(def declaration-specifiers (set/union storage-class-specifiers type-specifiers type-qualifiers))

;; Set of tokens that can begin a declarator
(def declarator-start-tokens
  #{:identifier :lparen :op_star})

;; Set of tokens that can begin a declarator suffix
(def declarator-suffix-start-tokens
  #{:lparen :lbracket})

;; Set of tokens that are literal values
(def literals
  #{:fp_literal :dec_literal :hex_literal :char_literal :string_literal})

;; Set of assignment operator tokens
(def assignment-operators
  #{:op_assign :op_lshift_assign :op_rshift_assign :op_bit_or_assign :op_bit_and_assign
    :op_bit_xor_assign :op_plus_assign :op_minus_assign :op_mul_assign :op_div_assign
    :op_mod_assign})

;; Unary operators
(def unary-operators
  #{:op_amp :op_star :op_plus :op_minus :op_bit_compl :op_not})

;; Increment and decrement operators
(def inc-dec-operators
  #{:op_inc :op_dec})

;; Tokens that can start a postfix expression suffix
(def postfix-suffix-start-tokens
  (set/union #{:lbracket :lparen :dot :op_arrow} inc-dec-operators))

;; Tokens that can start a declaration.
;; FIXME: need to handle typedef names.
(def declaration-start-tokens
  (set/union storage-class-specifier-tokens
             type-specifier-tokens
             type-qualifier-tokens
             #{:kw_struct :kw_union}))

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
   :op_amp 4, ; not called :op_bit_and because it also means "address of" 
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

;; "Punctuation" tokens, from lexer
(def c-punct-tokens (set (map #(second %) cl/c-punct-patterns)))

;; Set of punctuation tokens to discard for AST construction:
;; everything but ellipsis
(def c-punct-tokens-discard (disj c-punct-tokens :ellipsis))

;; Node types which should be eliminated from an AST when they
;; have only one child.
(def eliminate-when-one-child
  #{:expression :assignment_expression :conditional_expression
    :cast_expression :unary_expression :postfix_expression :primary_expression})


;; ----------------------------------------------------------------------
;; Forward declarations
;; ----------------------------------------------------------------------

(declare parse-cast-expression)
(declare parse-expression)
(declare parse-assignment-expression)
(declare parse-specifier-qualifier-list)
(declare parse-declarator)
(declare update-declarator-context)
(declare parse-type-specifier)
(declare parse-type-qualifier)
(declare parse-declaration-specifiers)
(declare parse-declaration)
(declare parse-statement)
(declare parse-compound-statement)


;; ----------------------------------------------------------------------
;; Functions
;; ----------------------------------------------------------------------


(def is-literal? (l/make-token-type-pred literals))

;; FIXME: this needs to handle typedef names
(def is-declaration-start? (l/make-token-type-pred declaration-start-tokens))


(defn parse-literal [token-seq ctx]
  ;(println "Parsing literal!")
  (if (empty? token-seq)
    (exc/throw-exception (l/format-err "Unexpected end of input" token-seq))
    (let [tt (l/next-token-type token-seq)]
      (if (not (l/next-token-matches? token-seq is-literal?))
        (exc/throw-exception (l/format-err (str "Expected literal, saw " tt) token-seq))
        (p/do-production :literal [(p/expect tt)] token-seq ctx)))))


(defn parse-primary-expression [token-seq ctx]
  (if (empty? token-seq)
    (exc/throw-exception "Unexpected end of input")
    (let [tt (l/next-token-type token-seq)]
      (case tt
        :lparen (p/do-production :primary_expression [(p/expect :lparen) parse-expression (p/expect :rparen)] token-seq ctx)
        :identifier (p/do-production :primary_expression [(p/expect :identifier)] token-seq ctx)
        (p/do-production :primary_expression [parse-literal] token-seq ctx)))))


(defn parse-argument-expression-list [token-seq ctx]
  ; parse initial argument expression
  (let [pr (p/do-production :argument_expression_list [parse-assignment-expression] token-seq ctx)
        remaining (:tokens pr)]
    ; See if there are more argument expressions
    (if (l/next-token-is? remaining :rparen)
      ; reached end, we're done
      pr
      ; there is at least one more argument expression
      (p/continue-production pr [(p/expect :comma) parse-argument-expression-list] token-seq ctx :flatten))))


(defn parse-opt-argument-expression-list [token-seq ctx]
  (if (l/next-token-is? token-seq :rparen)
    ; no argument expressions, so just apply epsilon production
    (p/do-production :opt_argument_expression_list [] token-seq ctx)
    ; there is at least one argument expression
    (p/do-production :opt_argument_expression_list [parse-argument-expression-list] token-seq ctx)))


(defn parse-postfix-suffix [token-seq ctx]
  (cond
    ; Array subscript
    (l/next-token-is? token-seq :lbracket)
    (p/do-production :array_subscript [(p/expect :lbracket) parse-expression (p/expect :rbracket)] token-seq ctx)
    
    ; Function call
    (l/next-token-is? token-seq :lparen)
    (p/do-production :function_call [(p/expect :lparen)
                                     parse-opt-argument-expression-list
                                     (p/expect :rparen)] token-seq ctx)
    
    ; Member access
    (l/next-token-is? token-seq :dot)
    (p/do-production :member_access [(p/expect :dot) (p/expect :identifier)] token-seq ctx)
    
    ; Indirect member access
    (l/next-token-is? token-seq :op_arrow)
    (p/do-production :indirect_member_access [(p/expect :op_arrow) (p/expect :identifier)] token-seq ctx)
    
    ; Post-increment
    (l/next-token-is? token-seq :op_inc)
    (p/do-production :post_increment [(p/expect :op_inc)] token-seq ctx)
    
    ; Post-decrement
    (l/next-token-is? token-seq :op_dec)
    (p/do-production :post_decrement [(p/expect :op_dec)] token-seq ctx)
    
    ; This should not happen
    :else (exc/throw-exception (l/format-err "Invalid postfix suffix" token-seq))))


(defn parse-postfix-suffix-list [token-seq ctx]
  (let [pr (p/do-production :postfix_suffix_list [parse-postfix-suffix] token-seq ctx)
        remaining (:tokens pr)]
    (if (l/next-token-in? remaining postfix-suffix-start-tokens)
      ; continue recursively
      (p/continue-production pr [parse-postfix-suffix-list] ctx :flatten)
      ; done
      pr)))


(defn parse-postfix-expression [token-seq ctx]
  (let [pr (p/do-production :postfix_expression [parse-primary-expression] token-seq ctx)
        remaining (:tokens pr)]
    (if (l/next-token-in? remaining postfix-suffix-start-tokens)
      ; parse one or more postfix suffixes
      (p/continue-production pr [parse-postfix-suffix-list] ctx)
      ; no postfix suffixes
      pr)))


;; TODO: should handle sizeof
(defn parse-unary-expression [token-seq ctx]
  (cond
    (l/next-token-in? token-seq unary-operators)
    (p/do-production :unary_expression [(p/expect (l/next-token-type token-seq))
                                        parse-cast-expression] token-seq ctx)
    
    (l/next-token-in? token-seq inc-dec-operators)
    (p/do-production :unary_expression [(p/expect (l/next-token-type token-seq))
                                        parse-unary-expression] token-seq ctx)
    
    :else
    (p/do-production :unary_expression [parse-postfix-expression] token-seq ctx)))


(defn parse-type-name [token-seq ctx]
  ; Current state is:
  ;    type-name -> ^ specifier-qualifier-list
  ;    type-name -> ^ specifier-qualifier-list abstract-declarator
  (let [pr (p/do-production :type_name [parse-specifier-qualifier-list] token-seq ctx)
        remaining (:tokens pr)]
    ; type names only appear in cast and sizeof expressions,
    ; and are always surrounding by parentheses.  So, if we
    ; look ahead and see a right paren, then there is definitely
    ; no abstract declarator.
    (if (l/next-token-is? remaining :rparen)
      ; We're done
      pr
      ; Parse an abstract declarator
      (p/continue-production pr [parse-declarator] (update-declarator-context ctx :allow_abstract)))))


;; TODO: just a place holder for now (allowing only integer literals)
(defn parse-cast-expression [token-seq ctx]
  (if (l/next-token-is? token-seq :lparen)
    (p/do-production :cast_expression [(p/expect :lparen)
                                       parse-type-name
                                       (p/expect :rparen)
                                       parse-cast-expression] token-seq ctx)
    (p/do-production :cast_expression [parse-unary-expression] token-seq ctx)))


(def c-infix-operators
  (p/make-operators binop-precedence binop-associativity parse-cast-expression))


(defn parse-logical-or-expression [token-seq ctx]
  (p/parse-infix-expression token-seq (assoc ctx :operators c-infix-operators)))


(defn parse-conditional-expression [token-seq ctx]
  ; Current state is:
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


(defn parse-constant-expression [token-seq ctx]
  (p/do-production :constant_expression [parse-conditional-expression] token-seq ctx))


(defn parse-assignment-expression [token-seq ctx]
  ; Current state is:
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


(defn parse-expression [token-seq ctx]
  ; Current state is:
  ;   expression -> ^ assignment-expression
  ;   expression -> ^ assignment-expression ',' expression
  (let [pr (p/do-production :expression [parse-assignment-expression] token-seq ctx)
        remaining (:tokens pr)]
    (if (l/next-token-is? remaining :comma)
      ; Production continues recursively
      (p/continue-production pr [(p/expect :comma) parse-expression] ctx :flatten)
      ; Production ends here
      (do
        ;(println "Tokens after parsing expression: " (:tokens pr))
        pr)
      )))


;; FIXME: does not handle
;;   - typedef name
;;   - enum specifier
(defn parse-specifier-qualifier-list [token-seq ctx]
  ; Current state is:
  ;    specifier-qualifier-list -> ^ type-specifier
  ;    specifier-qualifier-list -> ^ type-qualifier
  ;    specifier-qualifier-list -> ^ type-specifier specifier-qualifier-list
  ;    specifier-qualifier-list -> ^ type-qualifier specifier-qualifier-list
  (let [pr (p/do-production :specifier_qualifier_list
                            [(if (l/next-token-in? token-seq type-qualifier-tokens)
                               parse-type-qualifier
                               parse-type-specifier)] token-seq ctx)
        remaining (:tokens pr)]
    (if (l/next-token-in? remaining type-specifier-or-qualifier-start-tokens)
      ; continue recursively
      (p/continue-production pr [parse-specifier-qualifier-list] ctx :flatten)
      ; we're done
      pr)))

;; Update parsing context to allow concrete and/or abstract
;; declarators.  Ensures that neither :allow_concrete nor :allow_abstract
;; properties are left set unintentionally.
;;
;; Parameters:
;;   ctx - current parsing context (all properties will be preserved
;;         except for :allow_concrete and :allow_abstract)
;;   allowed - properties indicating which kinds of declarators
;;             are allowed (:allow_abstract and/or :allow_concrete)
;;
;; Returns:
;;   Updated parsing context which only allowed declarator types
;;   enabled.
;;
(defn update-declarator-context [ctx & allowed]
  (let [sanitized-context (dissoc ctx :allow_concrete :allow_abstract)]
    (into sanitized-context (map vector allowed (repeat true)))))


(defn parse-struct-declarator [token-seq ctx]
  ; Current state is:
  ;    struct-declarator -> ^ ':' constant-expression
  ;    struct-declarator -> ^ declarator ':' constant-expression
  ;    struct-declarator -> ^ declarator
  (if (l/next-token-is? token-seq :colon)
    ; Unnamed bitfield
    (p/do-production :struct_declarator [(p/expect :colon) parse-constant-expression] token-seq ctx)
    ; Named field: parse a named field (concrete declarator followed by
    ; optional bitfield)
    (let [pr (p/do-production :struct_declarator
                              [parse-declarator]
                              token-seq
                              (update-declarator-context ctx :allow_concrete))
          remaining (:tokens pr)]
      (if (l/next-token-is? remaining :colon)
        ; Parse bitfield
        (p/continue-production pr [(p/expect :colon) parse-constant-expression] ctx)
        ; No bitfield, so we're done
        pr))))


(defn parse-struct-declarator-list [token-seq ctx]
  (let [pr (p/do-production :struct_declarator_list [parse-struct-declarator] token-seq ctx)
        remaining (:tokens pr)]
    (if (l/next-token-is? remaining :comma)
      ; Continue recursively
      (p/continue-production pr [(p/expect :comma) parse-struct-declarator-list] ctx :flatten)
      ; No more declarators, so we're done
      pr)))


(defn parse-struct-declaration [token-seq ctx]
  (p/do-production :struct_declaration
                   [parse-specifier-qualifier-list parse-struct-declarator-list (p/expect :semicolon)]
                   token-seq
                   ctx))


(defn parse-struct-declaration-list [token-seq ctx]
  (let [pr (p/do-production :struct_declaration_list [parse-struct-declaration] token-seq ctx)
        remaining (:tokens pr)]
    ; See if there are more declarations
    (if (l/next-token-is? remaining :rbrace)
      ; No more declarations
      pr
      ; Continue recursively
      (p/continue-production pr [parse-struct-declaration-list] ctx))))


(defn complete-with-struct-declaration-list [pr remaining ctx]
  (p/continue-production pr [(p/expect :lbrace) parse-struct-declaration-list (p/expect :rbrace)] ctx))


(defn parse-struct-or-union-specifier [token-seq ctx]
  ; Current state is:
  ;   struct-or-union-specifier -> ^ struct-or-union '{' struct-declaration-list '}'
  ;   struct-or-union-specifier -> ^ struct-or-union identifier '{' struct-declaration-list '}'
  ;   struct-or-union-specifier -> ^ struct-or-union identifier
  (if (not (l/next-token-in? token-seq #{:kw_struct :kw_union}))
    (exc/throw-exception (l/format-err "Expected 'struct' or 'union'") token-seq)
    ; Start with just the 'struct' or 'union' keyword
    (let [pr (p/do-production :struct_or_union_specifier
                              [(p/expect (l/next-token-type token-seq))]
                              token-seq ctx)
          remaining (:tokens pr)]
      ; Struct declaration list follows immediately?
      (if (l/next-token-is? remaining :lbrace)
        ; Unnamed struct or union
        (complete-with-struct-declaration-list pr remaining ctx)
        ; Named struct or union, get the name
        (let [pr2 (p/continue-production pr [(p/expect :identifier)] ctx)
              remaining2 (:tokens pr2)]
          (if (l/next-token-is? remaining2 :lbrace)
            ; Has struct declaration list
            (complete-with-struct-declaration-list pr2 remaining2 ctx)
            ; No struct declaration list
            pr2))))))


(defn parse-type-specifier [token-seq ctx]
  (cond
    ; Single token type specifier?
    (l/next-token-in? token-seq type-specifier-tokens)
    (p/do-production :type_specifier [(p/expect (l/next-token-type token-seq))] token-seq ctx)
    
    ; Struct or union specifier?
    (l/next-token-in? token-seq #{:kw_struct :kw_union})
    (p/do-production :type_specifier [parse-struct-or-union-specifier] token-seq ctx)
    
    ; TODO:
    ;   - handle typedef names
    ;   - handle enum specifier
    :else (exc/throw-exception (l/format-err "Expected: type specifier" token-seq))))


(defn parse-type-qualifier [token-seq ctx]
  (if (not (l/next-token-in? token-seq type-qualifier-tokens))
    (exc/throw-exception (l/format-err "Expected type qualifier" token-seq))
    (p/do-production :type_qualifier [(p/expect (l/next-token-type token-seq))] token-seq ctx)))



(defn parse-storage-class-specifier [token-seq ctx]
  (if (not (l/next-token-in? token-seq storage-class-specifier-tokens))
    (exc/throw-exception (l/format-err "Expected storage class specifier" token-seq))
    (p/do-production :storage_class_specifier [(p/expect (l/next-token-type token-seq))] token-seq ctx)))


(defn parse-type-qualifier-list [token-seq ctx]
  (p/accept-matching :type_qualifier_list
                     (l/make-token-type-pred type-qualifier-tokens)
                     token-seq))


(defn parse-pointer [token-seq ctx]
  ; pointer -> ^ '*' type-qualifier-list
  ; pointer -> ^ '*' type-qualifier-list pointer
  (let [pr (p/do-production :pointer [(p/expect :op_star) parse-type-qualifier-list] token-seq ctx)
        remaining (:tokens pr)]
    (if (l/next-token-is? remaining :op_star)
      ; continue recursively
      (p/continue-production pr [parse-pointer] ctx)
      ; done
      pr)))


(defn parse-opt-pointer [token-seq ctx]
  (if (l/next-token-is? token-seq :op_star)
    (p/do-production :opt_pointer [parse-pointer] token-seq ctx)
    (p/do-production :opt_pointer [] token-seq ctx)))


(def is-lparen? #(= (l/get-token-type %) :lparen))


;; Note that this parses both abstract and concrete direct declarator bases.
;; In the context:
;;   - :allow_abstract indicates an abstract direct declarator base is allowed
;;   - :allow_concrete indicates a concrete direct declarator base is allowed
;;
;; The tricky part is to distinguish when to apply the epsilon production
;; (allowed for abstract declarators only), which is essentially equivalent to
;; the identifier in a concrete declarator.  The problem is that if we see
;; a left paren, it could be parentheses used for grouping, or it could be a
;; declarator suffix indicating a function.  Our strategy is that if we look
;; ahead and see either:
;;
;;    - '(' ')', or
;;    - '(' <token indicating the start of a type>
;;
;; then the left paren indicates the start of a declarator suffix, and we
;; should apply the epsilon production.
;;
;; Also, the epsilon production can be applied for an abstract declarator
;; if the :has_pointer property is set in the context, and no other
;; production makes sense.  This is for abstract declarators such as
;;
;;     *
;;
;; which might appear in a type name such as
;;
;;     int *
;;
(defn parse-direct-declarator-base [token-seq ctx]
  ; Current state is:
  ;   direct-declarator-base -> ^ identifier           -- concrete only
  ;   direct-declarator-base -> ^ '(' declarator ')'   -- either concrete or abstract
  ;   direct-declarator-base -> ^ epsilon              -- abstract only
  (cond
    (and (:allow_concrete ctx) (l/next-token-is? token-seq :identifier))
    (p/do-production :direct_declarator_base [(p/expect :identifier)] token-seq ctx)

    ; See if we can accept an abstract declarator and if
    ; we're looking ahead to a function suffix. If so, we can
    ; apply the epsilon production.
    (and (:allow_abstract ctx)
         (or (l/next-tokens-are? token-seq [:lparen :rparen])
             (l/next-tokens-match? token-seq [is-lparen? is-declaration-start?])))
    (p/do-production :direct_declarator_base [] token-seq ctx)
    
    ; At this point, a left parenthesis can only indicate grouping
    (l/next-token-is? token-seq :lparen)
    (p/do-production :direct_declarator_base [(p/expect :lparen)
                                              parse-declarator
                                              (p/expect :rparen)] token-seq ctx)
    
    ; At this point, we don't have a viable concrete direct declarator base,
    ; but we can still get a viable abstract one as long as there was
    ; a pointer.
    (and (:allow_abstract ctx) (:has_pointer ctx))
    (p/do-production :direct_declarator_base [] token-seq ctx)
    
    :else
    ; FIXME: need better error message
    (exc/throw-exception (l/format-err "No viable declarator base" token-seq))))


;; Parameter declarations!
;;
;; The K&R 2nd ed. grammar specifies these as:
;;   parameter-declaration -> ^ declaration-specifiers
;;   parameter-declaration -> ^ declaration-specifiers declarator
;;   parameter-declaration -> ^ declaration-specifiers abstract-declarator
;;
;; The ambiguity between the latter two productions would require
;; arbitrary lookahead to resolve.  Our solution is to use a single
;; parse function to parse both concrete and abstract declarators
;; and use the parser context to indicate which are allowed.
;;
;; Determining when to apply the first production is also somewhat
;; interesting: we parse the declaration specifiers, and after that
;; if the next token is ',' or ')', then there is no declarator
;; (concrete or abstract.)
;;
(defn parse-parameter-declaration [token-seq ctx]
  (let [pr (p/do-production :parameter_declaration [parse-declaration-specifiers] token-seq ctx)
        remaining (:tokens pr)]
    (if (l/next-token-in? remaining #{:comma :rparen})
      ; no declarator (concrete or abstract)
      pr
      ; there is a declarator
      (p/continue-production pr
                             [parse-declarator]
                             (update-declarator-context ctx :allow_concrete :allow_abstract)))))


(defn parse-parameter-list [token-seq ctx]
  ; Start by parsing one parameter declaration
  (let [pr (p/do-production :parameter_list [parse-parameter-declaration] token-seq ctx)
        remaining (:tokens pr)]
    ; See whether the parameter list continues
    (cond
      ; ',' '...' indicating varargs is parsed by parse-parameter-type-list,
      ; so stop here if we see them
      (l/next-tokens-are? remaining [:comma :ellipsis])
      pr
      
      ; Continue recursively?
      (l/next-token-is? remaining :comma)
      (p/continue-production pr [(p/expect :comma) parse-parameter-list] ctx :flatten)
      
      ; Stop here
      :else pr)))


(defn parse-parameter-type-list [token-seq ctx]
  (let [pr (p/do-production :parameter_type_list [parse-parameter-list] token-seq ctx)
        remaining (:tokens pr)]
    (if (l/next-token-is? remaining :comma)
      (p/continue-production pr [(p/expect :comma) (p/expect :ellipsis)] ctx :flatten)
      pr)))


;; FIXME: productions should be (from ANTLR 3 grammar)
;; declarator_suffix
;;     :   '[' constant_expression ']'
;;     |   '[' ']'
;;     |   '(' parameter_type_list ')'
;;     |   '(' identifier_list ')'          -- old style C, won't support this for now?
;;     |   '(' ')'                          -- old style C, won't support this for now?
;; 	;
;; This looks doable with 2 tokens of lookahead.
;; 
(defn parse-declarator-suffix [token-seq ctx]
  (cond
    (l/next-tokens-are? token-seq [:lbracket :rbracket])
    (p/do-production :declarator_suffix [(p/expect :lbracket) (p/expect :rbracket)] token-seq ctx)
    
    (l/next-token-is? token-seq :lbracket)
    (p/do-production :declarator_suffix [(p/expect :lbracket)
                                         parse-constant-expression
                                         (p/expect :rbracket)] token-seq ctx)
    
    :else
    (p/do-production :declarator_suffix [(p/expect :lparen)
                                         parse-parameter-type-list
                                         (p/expect :rparen)] token-seq ctx)
    ))


(defn parse-declarator-suffix-list [token-seq ctx]
  ; Start by parsing one declarator suffix.
  (let [pr (p/do-production :declarator_suffix_list [parse-declarator-suffix] token-seq ctx)
        remaining (:tokens pr)]
    (if (l/next-token-in? remaining declarator-suffix-start-tokens)
      ; Continue recursively.
      (p/continue-production pr [parse-declarator-suffix-list] ctx :flatten)
      ; Done.
      pr)))


(defn parse-opt-declarator-suffix-list [token-seq ctx]
  (if (l/next-token-in? token-seq declarator-suffix-start-tokens)
    (p/do-production :opt_declarator_suffix_list [parse-declarator-suffix-list] token-seq ctx)
    (p/do-production :opt_declarator_suffix_list [] token-seq ctx)))


;; Note that this parses both abstract and concrete direct declarators.
;; In the context:
;;   - :allow_abstract indicates an abstract direct declarator is allowed
;;   - :allow_concrete indicates a concrete direct declarator is allowed
;;
(defn parse-direct-declarator [token-seq ctx]
  (p/do-production :direct_declarator [parse-direct-declarator-base
                                       parse-opt-declarator-suffix-list] token-seq ctx))


;; Determine if an :opt_pointer node represents an actual pointer.
(defn has-pointer? [node]
  (node/has-children? node))

;; Note that this parses both abstract and concrete declarators.
;; In the context:
;;   - :allow_abstract indicates an abstract declarator is allowed
;;   - :allow_concrete indicates a concrete declarator is allowed
;; Note that we need to check whether a pointer was parsed, since this
;; affects which kinds of direct abstract declarator bases are allowed.
;;
(defn parse-declarator [token-seq ctx]
  ; Start by parsing an optional pointer.
  (let [pr (p/do-production :declarator [parse-opt-pointer] token-seq ctx)
        remaining (:tokens pr)
        ; the first child of the :declarator node is the :opt_pointer node
        opt-ptr-node (node/get-child (:node pr) 0)
        ; note whether the :opt_pointer node has an actual pointer
        ctx2 (assoc ctx :has_pointer (has-pointer? opt-ptr-node))]
    ; Now parse a direct declarator
    (p/continue-production pr [parse-direct-declarator] ctx2)))


(defn parse-initializer [token-seq ctx]
  (p/do-production :initializer [parse-assignment-expression] token-seq ctx))


(defn parse-while-statement [token-seq ctx]
  (p/do-production :while_statement [(p/expect :kw_while)
                                     (p/expect :lparen) parse-expression (p/expect :rparen)
                                     parse-statement] token-seq ctx))


(defn parse-if-statement [token-seq ctx]
  (p/do-production :if_statement [(p/expect :kw_if)
                                  (p/expect :lparen) parse-expression (p/expect :rparen)
                                  parse-statement] token-seq ctx))


(defn parse-labeled-statement [token-seq ctx]
  (p/do-production :labeled_statement [(p/expect :identifier) (p/expect :colon) parse-statement] token-seq ctx))


(defn parse-case-statement [token-seq ctx]
  (p/do-production :case_statement [(p/expect :kw_case) parse-expression (p/expect :colon) parse-statement] token-seq ctx))


(defn parse-default-statement [token-seq ctx]
  (p/do-production :default_statement [(p/expect :kw_default) (p/expect :colon) parse-statement] token-seq ctx))


(defn parse-if-statement [token-seq ctx]
  (let [pr (p/do-production :if_statement [(p/expect :kw_if)
                                           (p/expect :lparen) parse-expression (p/expect :rparen)
                                           parse-statement] token-seq ctx)
        remaining (:tokens pr)]
    ; See if there is an else clause
    (if (l/next-token-is? remaining :kw_else)
      ; Continue by parsing the else clause
      (p/continue-production pr [(p/expect :kw_else) parse-statement] ctx)
      ; No else clause, so we're done
      pr)))


(defn parse-switch-statement [token-seq ctx]
  (p/do-production :switch_statement [(p/expect :kw_switch)
                                      (p/expect :lparen) parse-expression (p/expect :rparen)
                                      parse-statement] token-seq ctx))


(defn parse-empty-statement [token-seq ctx]
  (p/do-production :empty_statement [(p/expect :semicolon)] token-seq ctx))


(defn parse-expression-statement [token-seq ctx]
  (println "Parsing expression statement")
  (p/do-production :expression_statement [parse-expression (p/expect :semicolon)] token-seq ctx))


(defn parse-statement [token-seq ctx]
  (if (empty? token-seq)
    (exc/throw-exception "Unexpected end of input")
    (let [tt (l/get-token-type (first token-seq))]
      (case tt
        :lbrace (p/do-production :statement [parse-compound-statement] token-seq ctx)
        :kw_while (p/do-production :statement [parse-while-statement] token-seq ctx)
        :kw_if (p/do-production :statement [parse-if-statement] token-seq ctx)
        :kw_switch (p/do-production :statement [parse-switch-statement] token-seq ctx)
        :kw_case (p/do-production :statement [parse-case-statement] token-seq ctx)
        :kw_default (p/do-production [parse-default-statement] token-seq ctx)
        :semicolon (p/do-production :statement [parse-empty-statement] token-seq ctx)
        ; Labeled statements require two tokens of lookahead
        (if (l/next-tokens-are? token-seq [:identifier :colon])
          (p/do-production :statement [parse-labeled-statement] token-seq ctx)
          ; Default is to parse an expression statement
          (p/do-production :statement [parse-expression-statement] token-seq ctx))))))


;; Another interesting point in the parser: distinguishing statements
;; from declarations (since we want to support the C99 feature
;; of allowing declarations and statements to appear in any order.)
(defn parse-block-item [token-seq ctx]
  (if (l/next-token-matches? token-seq is-declaration-start?)
    (p/do-production :block_item [parse-declaration] token-seq ctx)
    (p/do-production :block_item [parse-statement] token-seq ctx)))


(defn parse-block-item-list [token-seq ctx]
  (let [pr (p/do-production :block_item_list [parse-block-item] token-seq ctx)
        remaining (:tokens pr)]
    (if (l/next-token-is? remaining :rbrace)
      ; end of block item list
      pr
      ; there is at least one more block item
      (p/continue-production pr [parse-block-item-list] ctx :flatten))))


(defn parse-opt-block-item-list [token-seq ctx]
  (if (l/next-token-is? token-seq :rbrace)
    ; empty block
    (p/do-production :opt_block_item_list [] token-seq ctx)
    ; there is at least one block item
    (p/do-production :opt_block_item_list [parse-block-item-list] token-seq ctx)))


;; Parse compound statement (block).
;; Note that because C doesn't allow nested functions,
;; function definitions are not allowed inside
;; a block.
(defn parse-compound-statement [token-seq ctx]
  (p/do-production :compound_statement [(p/expect :lbrace)
                                        parse-opt-block-item-list
                                        (p/expect :rbrace)] token-seq (dissoc ctx :allow_func)))


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
(defn parse-init-declarator [token-seq ctx]
  ;; An init declarator must be concrete (not abstract)
  (let [pr (p/do-production :init_declarator
                            [parse-declarator]
                            token-seq
                            (update-declarator-context ctx :allow_concrete))
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


;; Return appropriate declaration specifier parse function
;; according to what the next token in the token sequence is.
(defn choose-declaration-specifier-parse-fn [token-seq]
  (cond
    (l/next-token-in? token-seq storage-class-specifier-tokens)
    parse-storage-class-specifier
    
    (l/next-token-in? token-seq type-qualifier-tokens)
    parse-type-qualifier
    
    :else
    parse-type-specifier))


(defn parse-declaration-specifiers [token-seq ctx]
  ; Current state is:
  ;    declaration-specifiers -> ^ storage-class-specifier
  ;    declaration-specifiers -> ^ type-specifier
  ;    declaration-specifiers -> ^ type-qualifier
  ;    declaration-specifiers -> ^ storage-class-specifier declaration-specifiers
  ;    declaration-specifiers -> ^ type-specifier declaration-specifiers
  ;    declaration-specifiers -> ^ type-qualifier declaration-specifiers
  (let [pr (p/do-production :declaration_specifiers
                            [(choose-declaration-specifier-parse-fn token-seq)]
                            token-seq ctx)
        remaining (:tokens pr)]
    ; How to tell if declaration specifiers continue?
    ; If we look ahead and see a storage class specifier, a type qualifier,
    ; or the start of a type specifier, then yes.  Otherwise, no.
    (if (l/next-token-in? remaining declaration-start-tokens)
      ; continue recursively
      (p/continue-production pr [parse-declaration-specifiers] ctx :flatten)
      ; we're done
      pr)))


(defn parse-init-declarator-list [token-seq ctx]
  ; Current state is:
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


(defn parse-opt-init-declarator-list [token-seq ctx]
  ; do we see a declarator?
  (if (l/next-token-in? token-seq declarator-start-tokens)
    ; there is at least one declarator
    (p/do-production :opt_init_declarator_list [parse-init-declarator-list] token-seq ctx)
    ; no declarators
    (p/do-production :opt_init_declarator_list [] token-seq ctx)))


; Check whether given node is a function definition.
(defn is-function-definition? [node]
  (= (:symbol node) :function_definition))


(defn parse-declaration [token-seq ctx]
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
      (p/continue-production pr [(p/expect :semicolon)] ctx))))


(defn parse-declaration-list [token-seq ctx]
  ; Current state is:
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
      (p/continue-production pr [parse-declaration-list] ctx :flatten))))


;; Parse specified token sequence as a translation unit,
;; returning a parse tree.
;;
;; Parameters:
;;   token-seq - input token sequence
;;
;; Returns:
;;   root of parse tree; throws exception if the token sequence is
;;   not a valid translation unit
;;
(defn parse [token-seq]
  (:node (parse-declaration-list token-seq {:allow_func true})))


;; Node predicate for determining which node should be
;; retained in the AST.
;;
;; Parameters:
;;   n - a parse tree Node
;;
;; Returns:
;;   true if the node should be retained in the AST, false if
;;   it should be discarded
;;
(defn c-node-filter [n]
  (not (contains? c-punct-tokens-discard (:symbol n))))


;; Translate tree to get rid of unnecessary one-child nonterminal
;; nodes.  Used for AST construction.
;;
;; Parameters:
;;   n - a tree node
;;
;; Returns:
;;   simplified tree without the redundant one-child nonterminal nodes
;;
(defn simplify-tree [n]
  (let [sym (:symbol n)
        nchildren (node/num-children n)]
    (cond
      ; Nothing to do if there are no children
      (= nchildren 0) n
      
      ; If there's one child, and the node type is one of the ones
      ; to eliminate in the one-child case, recur on the child
      (and (= nchildren 1) (contains? eliminate-when-one-child sym))
      (recur (node/get-child n 0))
      
      ; This node has children, but either it has more than one,
      ; or it's not one that should be eliminated.  Return a
      ; translated version in which the simplification is
      ; performed recursively on the children.
      :else (node/replace-children n (map simplify-tree (node/children n))))))


;; Transform a parse tree into an abstract syntax tree.
;;
;; Parameters:
;;   pt - a parse tree
;;
;; Returns:
;;   an abstract syntax tree
;;
(defn to-ast [pt]
  (simplify-tree (node/filter-tree pt c-node-filter)))


;; ----------------------------------------------------------------------
;; Just for testing...
;; ----------------------------------------------------------------------

; int x, y;


(def testprog
"int main(void) {
    printf(\"Hello, world!\\n\");
}"
)

;"
;struct Point {
;    int x, y;
;};
;
;int f(int, double x, float (y))
;{
;    int q;
;    h;
;}
;"

;int x;
;char *p;
;double *q[];
;int a = 2 + 3;
;int b = 42 + 1 << 5;
;long c = (long) 17;
;int d = 8989 & 3;
;int e = ~-15;

(def token-seq (l/token-sequence (cl/create-from-string testprog)))
(def t (parse token-seq))

;; Create AST from the parse tree.
(def ast (to-ast t))
