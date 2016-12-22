(ns catparty.parser
  (:require [catparty.lexer :as lexer]
            [catparty.clexer :as clexer]
            [catparty.node :as node]
            [catparty.exc :as exc]))

;; Recursive descent and precedence climbing parser routines.

; ------------------------------------------------------------
; Data types
; ------------------------------------------------------------

; Result of expanding a single right-hand-side symbol:
; A single parse node, and a sequence containing the remaining
; input tokens.
(defrecord SingleParseResult [node tokens])

; Result of partially or completely applying a production:
; A sequence of 0 or more parse nodes, and a sequence containing
; the remaining input tokens.
(defrecord ParseResult [nodes tokens])

; ------------------------------------------------------------
; Functions
; ------------------------------------------------------------

; Create an initial ParseResult.
; Useful for beginning a production (as part of a call to
; apply-production).
;
; Parameters:
;   token-seq - the input token sequence
;
; Returns: an initial (empty) ParseResult with the given input
; token sequence.
;
(defn initial-parse-result [token-seq]
  (ParseResult. [] token-seq))

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

; Create a symbol application function to expect and consume a particular
; type of token (i.e., a terminal symol).
;
; Parameters:
;   expected-token-type - a keyword specifying the expected token
;                         type (e.g., :identifier)
;
; Returns: a symbol application function which takes an input tokens
; sequence and returns a SingleParseResult with the resulting
; terminal parse node and the remaining input tokens.
;
(defn expect [expected-token-type]
  (fn [token-seq]
    ; Check to see if there are more tokens
    (if (empty? token-seq)
      ; No more tokens
      (exc/throw-exception "Unexpected end of input")
      (let [next-token (first token-seq)]
        ; Check whether next token matches expected token type
        (if (not (lexer/token-is-type? next-token expected-token-type))
          ; Wrong token type seen
          (exc/throw-exception (str "Expected "
                                    expected-token-type
                                    ", saw "
                                    (lexer/get-token-type next-token)))
          ; Consume the token and return a SingleParseResult
          (SingleParseResult. (make-terminal-node next-token) (rest token-seq)))))))

;; Return a SingleParseResult with a single node (labeled with
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
  ; Function to construct the SingleParseResult with the children
  ; and remaining tokens.
  (let [make-result (fn [children remaining]
                      (SingleParseResult. (node/make-node symbol children) remaining))]
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

; Apply a production (or part of a production) by expanding
; symbols on the right-hand side of a production.
;
; Parameters:
;   parse-result - the ParseResult to which the expanded symbols
;                  should be added; it also contains the current token sequence
;   rhs          - is a sequence of symbol application functions (corresponding
;                  to the symbols on the right hand side of the production being
;                  applied)
;
; Returns: a ParseResult containing the parse nodes resulting from
; applying the symbol application functions, and the remaining
; input tokens.
;
(defn apply-production [parse-result rhs]
  (loop [result parse-result
         symbol-application-functions rhs]
    (if (empty? symbol-application-functions)
      ; Done, no more symbol application functions to apply
      result
      ; Apply the next symbol application function and continue
      (let [symbol-application-function (first symbol-application-functions)
            single-parse-result (symbol-application-function (:tokens result))]
        (recur (ParseResult. (conj (:nodes result) (:node single-parse-result)) (:tokens single-parse-result))
               (rest symbol-application-functions))))))

; Complete a production by creating a SingleParseResult from a
; ParseResult, labeling it with a specified nonterminal symbol.
;
; Parameters:
;    nonterminal  - a keyword specifying the nonterminal symbol (e.g., :statement_list) 
;    parse-result - a ParseResult containing the results of applying zero or
;                   more symbol application functions
;
; Returns: a SingleParseResult
;
(defn complete-production [nonterminal parse-result]
  (SingleParseResult. (node/make-node nonterminal (:nodes parse-result)) (:tokens parse-result)))

; Perform a complete production, returning a SingleParseResult
; as a result.
;
; Parameters:
;   nonterminal - the left-hand nonterminal symbol of the production
;   rhs         - a sequence of symbol application functions (i.e., the right-hand
;                 side of the production)
;   token-seq   - the token sequence to parse
;
; Returns: a SingleParseResult
;
(defn do-production [nonterminal rhs token-seq]
  (complete-production nonterminal (apply-production (initial-parse-result token-seq) rhs)))
