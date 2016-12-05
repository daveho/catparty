(ns catparty.parser
  (:require [catparty.clexer :as clexer]
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
      ; Check to see if the next token has the expected type
      (let [[lexeme token-type lnum cnum] (first token-seq)]
        (if (not (= token-type expected-token-type))
          ; Wrong token type seen
          (exc/throw-exception (str "Expected " expected-token-type ", saw " token-type))
          ; Consume the token and return a SingleParseResult
          (SingleParseResult. (node/make-node-with-props token-type
                                                         lexeme
                                                         {:lnum lnum, :cnum cnum})
                              (rest token-seq)))))))

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
