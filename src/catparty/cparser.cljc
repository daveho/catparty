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

(def type-specifiers-and-qualifiers (set/union type-specifiers type-qualifiers))

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

;; Tokens that can start a postfix suffix
(def postfix-suffix-start-tokens
  (set/union #{:lbracket :lparen :op_dot :op_arrow} inc-dec-operators))

;; Tokens that can start a declaration.
;; FIXME: need to handle typedef names.
(def declaration-start-tokens
  (set/union type-specifiers-and-qualifiers #{:kw_struct :kw_union}))

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


(def is-literal? (l/make-token-type-pred literals))


(defn parse-literal [token-seq ctx]
  ;(println "Parsing literal!")
  (if (not (l/next-token-matches? token-seq is-literal?))
    (exc/throw-exception "Expected literal")
    (p/do-production :literal [(p/expect (l/next-token-type token-seq))] token-seq ctx)))


(declare parse-cast-expression)
(declare parse-expression)
(declare parse-assignment-expression)


;; FIXME: just a placeholder for now, only allows literals
(defn parse-primary [token-seq ctx]
  (p/do-production :primary [parse-literal] token-seq ctx))


(defn parse-argument-expression-list [token-seq ctx]
  ; parse initial argument expression
  (let [pr (p/do-production :argument_expression_list [parse-assignment-expression] token-seq ctx)
        remaining (:tokens pr)]
    ; See if there are more argument expressions
    (if (l/next-token-is? remaining :rparen)
      ; reached end, we're done
      pr
      ; there is at least one more argument expression
      (p/continue-production pr [(p/expect :comma) parse-argument-expression-list] token-seq ctx))))


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
    (l/next-token-is? token-seq :op_dot)
    (p/do-production :member_access [(p/expect :op_dot) (p/expect :identifier)] token-seq ctx)
    
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
    :else (exc/throw-exception "Invalid postfix suffix")))


(defn parse-postfix-suffix-list [token-seq ctx]
  (let [pr (p/do-production :postfix_suffix_list [parse-postfix-suffix] token-seq ctx)
        remaining (:tokens pr)]
    (if (l/next-token-in? remaining postfix-suffix-start-tokens)
      ; continue recursively
      (p/continue-production pr [parse-postfix-suffix-list] ctx)
      ; done
      pr)))


(defn parse-postfix-expression [token-seq ctx]
  (let [pr (p/do-production :postfix_expression [parse-primary] token-seq ctx)
        remaining (:tokens pr)]
    (if (l/next-token-in? token-seq postfix-suffix-start-tokens)
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


;; FIXME: this is just a placeholder fo now: it just parses a sequence of
;; type specifiers and type qualifiers
(defn parse-type-name [token-seq ctx]
  (p/accept-matching :type_name (l/make-token-type-pred type-specifiers-and-qualifiers) token-seq))


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
  (let [ctx2 (assoc ctx :operators c-infix-operators)]
    (let [pr (p/parse-infix-expression token-seq ctx2)]
      (do
        ;(println "After infix parsing: " (:tokens pr))
        pr
        )
      )))


(declare parse-expression)


(defn parse-conditional-expression [token-seq ctx]
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


(defn parse-assignment-expression [token-seq ctx]
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


(defn parse-expression [token-seq ctx]
  ; State is:
  ;   expression -> ^ assignment-expression
  ;   expression -> ^ assignment-expression ',' expression
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


(defn parse-type-qualifier-list [token-seq ctx]
  (p/accept-matching :type_qualifier_list
                     (l/make-token-type-pred type-qualifiers)
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


(declare parse-declarator)


(defn parse-direct-declarator-base [token-seq ctx]
  ; direct-declarator-base -> identifier
  ; direct-declarator-base -> '(' declarator ')'
  (if (l/next-token-is? token-seq :identifier)
    (p/do-production :direct_declarator_base [(p/expect :identifier)] token-seq ctx)
    (p/do-production :direct_declarator_base [(p/expect :lparen)
                                              parse-declarator
                                              (p/expect :rparen)] token-seq ctx)))


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
(defn parse-declarator-suffix [token-seq ctx]
  (if (l/next-token-is? token-seq :lparen)
    (p/do-production :declarator_suffix [(p/expect :lparen) (p/expect :rparen)] token-seq ctx)
    (p/do-production :declarator_suffix [(p/expect :lbracket) (p/expect :rbracket)] token-seq ctx)))


(defn parse-declarator-suffix-list [token-seq ctx]
  ; Start by parsing one declarator suffix.
  (let [pr (p/do-production :declarator_suffix_list [parse-declarator-suffix] token-seq ctx)
        remaining (:tokens pr)]
    (if (l/next-token-in? remaining declarator-suffix-start-tokens)
      ; Continue recursively.
      (p/continue-production pr [parse-declarator-suffix-list] ctx)
      ; Done.
      pr)))


(defn parse-opt-declarator-suffix-list [token-seq ctx]
  (if (l/next-token-in? token-seq declarator-suffix-start-tokens)
    (p/do-production :opt_declarator_suffix_list [parse-declarator-suffix-list] token-seq ctx)
    (p/do-production :opt_declarator_suffix_list [] token-seq ctx)))


(defn parse-direct-declarator [token-seq ctx]
  (p/do-production :direct_declarator [parse-direct-declarator-base
                                     parse-opt-declarator-suffix-list] token-seq ctx))


(defn parse-declarator [token-seq ctx]
  (p/do-production :declarator [parse-opt-pointer parse-direct-declarator] token-seq ctx))


(defn parse-initializer [token-seq ctx]
  (p/do-production :initializer [parse-assignment-expression] token-seq ctx))


(declare parse-declaration)
(declare parse-statement)
(declare parse-compound-statement)


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


(defn parse-statement [token-seq ctx]
  (cond
    (l/next-token-is? token-seq :lbrace) (p/do-production :statement [parse-compound-statement] token-seq ctx)
    (l/next-token-is? token-seq :kw_while) (p/do-production :statement [parse-while-statement] token-seq ctx)
    (l/next-token-is? token-seq :kw_if) (p/do-production :statement [parse-while-statement] token-seq ctx)
    (l/next-tokens-are? token-seq [:identifier :colon]) (p/do-production [parse-labeled-statement] token-seq ctx)
    (l/next-token-is? token-seq :kw_case) (p/do-production :statement [parse-case-statement] token-seq ctx)
    (l/next-token-is? token-seq :hw_default) (p/do-production [parse-default-statement] token-seq ctx)
    (l/next-token-is? token-seq :semicolon) (p/do-production :statement [(p/expect :semicolon)] token-seq ctx)
    :else (exc/throw-exception "Unknown statement type")
  ))
;  (p/do-production :statement [(p/expect :semicolon)] token-seq ctx))


;; Another interesting point in the parser: distinguishing statements
;; from declarations (since we want to support the C99 feature
;; of allowing declarations and statements to appear in any order.)
(defn parse-block-item [token-seq ctx]
  (if (l/next-token-in? token-seq declaration-start-tokens)
    (p/do-production :block_item [parse-declaration] token-seq ctx)
    (p/do-production :block_item [parse-statement] token-seq ctx)))


(defn parse-block-item-list [token-seq ctx]
  (let [pr (p/do-production :block_item_list [parse-block-item] token-seq ctx)
        remaining (:tokens pr)]
    (if (l/next-token-is? remaining :rbrace)
      ; end of block item list
      pr
      ; there is at least one more block item
      (p/continue-production pr [parse-block-item-list] ctx))))


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
  (let [pr (p/do-production :init_declarator [parse-declarator] token-seq ctx)
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


(defn parse-declaration-specifiers [token-seq ctx]
  (p/accept-matching :declaration_specifiers
                     (l/make-token-type-pred declaration-specifiers)
                     token-seq))


(defn parse-init-declarator-list [token-seq ctx]
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


(defn parse-opt-init-declarator-list [token-seq ctx]
  ; do we see a declarator?
  (if (l/next-token-in? token-seq declarator-start-tokens)
    ; there is at least one declarator
    (p/do-production :opt_init_declarator_list [parse-init-declarator-list] token-seq ctx)
    ; no declarators
    (p/do-production :opt_init_declarator_list [] token-seq)))


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

(def testprog
"
int x;
char *p;
double *q[];
int a = 2 + 3;
int f()
{
    int q;
    ;
}
int b = 42 + 1 << 5;
long c = (long) 17;
int d = 8989 & 3;
int e = ~-15;
")


(def token-seq (l/token-sequence (cl/create-from-string testprog)))
(def t (parse token-seq))
