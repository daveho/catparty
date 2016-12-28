(ns catparty.parser
  (:require [catparty.lexer :as l]
            [catparty.node :as node]
            [catparty.exc :as exc]))

;; Recursive descent and precedence climbing parser routines.

;; ------------------------------------------------------------
;; Data types
;; ------------------------------------------------------------

;; Result of partially or completely applying a production:
;; a parse node, and a sequence containing the remaining input tokens.
(defrecord ParseResult [node tokens])


;; ------------------------------------------------------------
;; Recursive descent parsing functions
;; ------------------------------------------------------------

;; Create an initial ParseResult.
;; Useful for beginning a production (as part of a call to
;; continue-production).
;;
;; Parameters:
;;   symbol    - the grammar symbol to label the parse node with
;;   token-seq - the input token sequence
;;
;; Returns: an initial (empty) ParseResult with the given
;; symbol and input token sequence.
;;
(defn initial-parse-result [symbol token-seq]
  (ParseResult. (node/make-node symbol []) token-seq))


;; Create a parse node for specified token.
;;
;; Parameters:
;;   token - a token
;;
;; Returns:
;;   a parse node representing the token
;;
(defn make-terminal-node [token]
  (let [[lexeme token-type lnum cnum] token]
    (node/make-node-with-props token-type
                               lexeme
                               {:lnum lnum, :cnum cnum})))


;; Create a symbol application function to expect and consume a particular
;; type of token (i.e., a terminal symol).
;;
;; Parameters:
;;   expected-token-type - a keyword specifying the expected token
;;                         type (e.g., :identifier)
;;
;; Returns: a symbol application function which takes an input tokens
;; sequence and returns a ParseResult with the resulting
;; terminal parse node and the remaining input tokens.
;;
(defn expect [expected-token-type]
  (fn [token-seq & [ctx]]
    ; Check to see if there are more tokens
    (if (empty? token-seq)
      ; No more tokens
      (exc/throw-exception "Unexpected end of input")
      (let [next-token (first token-seq)]
        ; Check whether next token matches expected token type
        (if (not (l/token-is-type? next-token expected-token-type))
          ; Wrong token type seen
          (exc/throw-exception (str "Expected "
                                    expected-token-type
                                    ", saw "
                                    (l/get-token-type next-token)))
          ; Consume the token and return a ParseResult
          (ParseResult. (make-terminal-node next-token) (rest token-seq)))))))


;; Return a ParseResult with a single node (labeled with
;; specified symbol) having as children one or more terminal nodes
;; representing tokens matched by given token predicate.
;;
;; Parameters:
;;   symbol - nonterminal symbol labeling the returned parse node
;;   token-pred - predicate function to select token(s) to add
;;                as children of the returned parse node
;;   token-seq - sequence of input tokens
;;
;; Returns:
;;   parse node labeled with specified symbol and terminal
;;   nodes for all matching tokens as children
;;
(defn accept-matching [symbol token-pred token-seq]
  ; Function to construct the ParseResult with the children
  ; and remaining tokens.
  (let [make-result (fn [children remaining]
                      (ParseResult. (node/make-node symbol children) remaining))]
    ; Match tokens until either there are no more tokens,
    ; or we encounter a non-matching token.
    (loop [remaining token-seq
           acc []]
      (if (empty? remaining)
        ; No more tokens, so we're done
        (make-result acc remaining)
        ; Get the next token
        (let [next-tok (first remaining)]
          (if (not (token-pred next-tok))
            ; Next token doesn't match, so we're done
            (make-result acc remaining)
            ; Create a node for the matched token, add it to the accumulator,
            ; and continue recursively.
            (recur (rest remaining) (conj acc (make-terminal-node next-tok)))))))))


;; Extend a ParseResult by adding the result of applying a
;; right hand side symbol application function.
;; The result's node will have the right hand side's node as a
;; new child, and will have whatever remaining tokens the
;; right hand side has.
;;
;; Parameters:
;;   pr - the overall ParseResult (the result of partially applying a production)
;;   rhs-result - a ParseResult from a right hand side
;;                symbol application function
;;
;; Returns:
;;   a new overall ParseResult in which the right hand side
;;   result is incoporated into the overall result
;;
(defn extend-parse-result [pr rhs-result]
  (let [parent (:node pr)
        child (:node rhs-result)
        remaining-tokens (:tokens rhs-result)]
    (ParseResult. (node/add-child parent child) remaining-tokens)))


;; Relabel given ParseResult's Node with the specified symbol.
;; This is useful for productions where a single parse function
;; can return distinct kinds of parse trees because of
;; ambiguities that can't be resolved with fixed lookahead.
;; I'm looking at you, C (function definition vs. declaration in
;; the context of an external declaration.)
;;
;; Parameters:
;;   pr - a ParseResult
;;   symbol - a nonterminal symbol
;;
;; Returns:
;;   a ParseResult in which the Node is relabeled with
;;   the new symbol
;;
(defn relabel-parse-result [pr symbol]
  (assoc pr :node (node/relabel (:node pr) symbol)))


;; Apply a production (or part of a production) by expanding
;; symbols on the right-hand side of a production.
;;
;; Parameters:
;;   parse-result - the ParseResult to which the expanded symbols
;;                  should be added; it also contains the current token sequence
;;   rhs          - is a sequence of symbol application functions (corresponding
;;                  to the symbols on the right hand side of the production being
;;                  applied)
;;   ctx (optional) - the parsing context (if one is being used)
;;
;; Returns: a ParseResult with a ParseNode containing the children
;; resulting from applying the symbol application functions, and
;; the remaining input tokens.
;;
(defn continue-production [parse-result rhs & [ctx]]
  (let [symbol (:symbol parse-result)]
    (loop [result parse-result
           fns rhs]
      (if (empty? fns)
        ; Done, no more symbol application functions to apply
        result
        ; Apply the next symbol application function and continue
        (let [rhs-fn (first fns)
              rhs-result (rhs-fn (:tokens result) ctx)]
          (recur (extend-parse-result result rhs-result)
                 (rest fns)))))))


;; Apply a complete or partial production, returning a ParseResult
;; as a result.
;;
;; Parameters:
;;   nonterminal - the left-hand nonterminal symbol of the production
;;   rhs         - a sequence of symbol application functions (i.e., the right-hand
;;                 side of the production)
;;   token-seq   - the token sequence to parse
;;   ctx (optional) - the parsing context (if one is being used)
;;
;; Returns: a ParseResult
;;
(defn do-production [nonterminal rhs token-seq & [ctx]]
  (continue-production (initial-parse-result nonterminal token-seq) rhs ctx))


;; ------------------------------------------------------------
;; Precedence climbing (for parsing infix expressions)
;; ------------------------------------------------------------

;; Record type for customization of the precedence climbing parser.
;; An instance of this type needs to be stored in the parsing context,
;; as the value of the :operators key.
;;
;; Fields are:
;;   precedence - map of operators to their precedence
;;   associativity - map of operators to their associativity (:left or :right)
;;   parse-primary - parse function for parsing a primary expression
;;
(defrecord Operators [precedence associativity parse-primary])


;; Check whether given token is an operator.
(defn is-operator? [token ops]
  (let [token-type (l/get-token-type token)]
    (contains? (:precedence ops) token-type)))


;; This is adapted more or less directly from the wikipedia pseudo code:
;;   http://en.wikipedia.org/wiki/Operator-precedence_parser

(defn need-recursive-parse? [token token-seq ops]
  ; Check whether
  ;   "the next token is a binary operator whose precedence is greater
  ;   than op's, or a right-associative operator whose precedence is equal to op's"
  (if (not (l/next-token-matches? token-seq (fn [t] (is-operator? t ops))))
    ; Either we've reached the end of the input token sequence,
    ; or the next token isn't an operator.
    false
    ; Check the precedence and associativity of the next token.
    (let [precedence (:precedence ops)
          associativity (:associativity ops)
          op-token-type (l/get-token-type token)
          op-prec (get precedence op-token-type)
          next-token-type (l/get-token-type (first token-seq))
          next-prec (get precedence next-token-type)
          next-assoc (get associativity next-token-type)]
      (or (> next-prec op-prec)
          (and (>= next-prec op-prec) (= :right next-assoc))))))


(declare parse-expression-1)


(defn parse-rhs [op rhs-result token-seq ctx]
  (let [token-seq (:tokens rhs-result)
        ops (:operators ctx)
        precedence (:precedence ops)]
    (if (not (need-recursive-parse? op token-seq ops))
      ; We can continue at the same precedence level,
      ; so rhs is fine as-is
      rhs-result
      ; Recursive parsing is needed at a higher precedence level
      ; (or we encountered a right-associative operator).
      (let [next-token (first token-seq)
            lookahead-token-type (l/get-token-type next-token)
            lookahead-prec (get precedence lookahead-token-type)]
        (parse-expression-1 rhs-result lookahead-prec token-seq) ctx))))


(defn parse-expression-1 [init-lhs-result min-precedence init-token-seq ctx]
  (let [ops (:operators ctx)
        parse-primary (:parse-primary ops)]
    (loop [lhs-result init-lhs-result
           token-seq init-token-seq]
      ; See whether next token is an operator
      (if (not (l/next-token-matches? token-seq (fn [t] (is-operator? t ops))))
        ; Done, return lhs ParseResult
        lhs-result
        ; Get op and parse next primary expression
        (let [op (first token-seq)
              next-primary-result (parse-primary (rest token-seq) ctx)
              ; Parsing the rhs is done in a separate function,
              ; which may involve recursive calls to parse-expression-1.
              rhs-result (parse-rhs op next-primary-result (:tokens next-primary-result) ctx)
              remaining (:tokens rhs-result)]
          ; Combine lhs and rhs and continue parsing at the same precedence level
          (let [n (node/make-node (l/get-token-type op) [(:node lhs-result) (:node rhs-result)])]
            (recur (ParseResult. n remaining) remaining)))))))


;; Parse an infix expression using precedence climbing.
;;
;; Parameters:
;;   token-seq - sequence of tokens to be parsed as an infix expression
;;   ctx - the parsing context, which must contain an :operators entry
;;         which refers to an instance of Operators (for operator
;;         precedence map, operator associativity map, parse primary
;;         function, etc.)
;;
;; Returns:
;;   ParseResult with the result of parsing the infix expression
;;
(defn parse-infix-expression [token-seq ctx]
  (let [ops (:operators ctx)
        parse-primary (:parse-primary ops)
        lhs-result (parse-primary token-seq ctx)]
    (parse-expression-1 lhs-result 0 (:tokens lhs-result) ctx)))
