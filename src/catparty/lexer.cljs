(ns catparty.lexer
  (:require [catparty.exc :as exc]))

;; Generic lexer module.
;; All that language-specific lexers need to provide is a sequence
;; of patterns.  Each pattern is a regex and a token type.

;; Fields are:
;;    lineseq - sequence of input lines remaining after current line
;;    line - current line
;;    tok - next token to be returned
;;    pats - sequence of patterns (pairs of regex and token type)
;;    lnum - line number (starts at 1 for first line)
;;    cnum - char number within line (starts at 1 for first character)
(defrecord Lexer [lineseq line tok pats lnum cnum])

; Attempt to ensure that the lexer has a line available
(defn fill-line [lexer]
  (cond
    ; If there is already a line, there's nothing to do
    (not (empty? (:line lexer))) lexer
    ; If there are no more input lines, then set line to nil (to signal EOF)
    (empty? (:lineseq lexer)) (assoc lexer :line nil)
    ; Get the next line
    :else (let [lines (:lineseq lexer)]
            (assoc lexer
              :lineseq (rest lines)
              :line (first lines)
              :lnum (inc (:lnum lexer))
              :cnum 1))))

(defn recognize-token [lexer]
  ; Trim leading whitespace
  (let [raw-line (:line lexer)               ; raw line before trimming leading ws
        line (clojure.string/triml raw-line) ; line after trimming leading ws
        ntrim (- (count raw-line) (count line)) ; number of ws characters trimmed
        cnum (+ (:cnum lexer) ntrim)         ; character number in line of start of token
        ]
    ; Attempt to match each token pattern in sequence
    (loop [patterns (:pats lexer)]
      ; If there are no more patterns, then the input contains an illegal token
      (if (empty? patterns)
        (exc/throw-exception (str "Illegal token at " line))
        (let [[token-regexp token-type] (first patterns)
              match (re-find token-regexp line)]
          (if match
            ; Current pattern is a match: update lexer's line and token
            (let [match-text (if (vector? match) (get match 0) match)
                  rest-of-line (subs line (count match-text))]
              (assoc lexer
                :line rest-of-line
                :tok [match-text token-type (:lnum lexer) cnum]
                :cnum (+ cnum (count match-text))))
            ; Current pattern is not a match: try next pattern
            (recur (rest patterns))))))))

; Attempt to ensure that the lexer has the next token available
(defn fill-token [lexer]
  (cond
    ; If there is already a token, there's nothing to do
    (not (nil? (:tok lexer))) lexer
    ; If there are no more lines, then leave tok nil (to signal end of stream)
    (nil? (:line lexer)) lexer
    ; If current line is empty, read another line and recur
    (empty? (clojure.string/trim (:line lexer))) (recur (fill-line lexer))
    ; A nonempty line is available: call recognize-token
    :else (recognize-token lexer)))

(defn create-from-lines
  "Create a lexer from a sequence of lines and sequence of patterns."
  [lines pats]
  (fill-token (Lexer. lines "" nil pats 0 0)))

(defn create-from-string
  "Create a lexer from an input string and sequence of patterns."
  [s pats]
  (let [lines (clojure.string/split s #"\n")]
    (create-from-lines lines pats)))

; Determine whether lexer is at end of file.
(defn at-eof [lexer]
  (nil? (:tok lexer)))

; Get the current token.
(defn get-current-token [lexer]
  (:tok lexer))

; Consume current token, returning updated lexer.
(defn consume-token [lexer]
  (fill-token (assoc lexer :tok nil)))

; Use given lexer to create a lazy sequence of tokens.
; This is the recommended way of using a lexer.
(defn token-sequence [lexer]
  (if (at-eof lexer)
    []
    (let [first-token (get-current-token lexer)
          advanced-lexer (consume-token lexer)]
      (cons first-token (lazy-seq (token-sequence advanced-lexer))))))

; Get the next token from a token sequence, throwing
; an exception if the token sequence is empty.
;
; Parameters:
;   token-seq - the input token sequence
;
; Returns: the next token, which is a vector containing the lexeme,
; token type, line number, and character number
; (e.g., ["foobar" :identifier 5 11])
;
(defn next-token [token-seq]
  (if (empty? token-seq)
    (exc/throw-exception "Unexpected end of input")
    (first token-seq)))

; Check to see whether the next token matches the specified
; predicate.
;
; Parameters:
;   token-seq - the input token sequence
;   pred - a token predicate
;
; Returns:
;   true if there is at least one more token and the predicate
;   returns true for the token, false otherwise
;
(defn next-token-matches? [token-seq pred]
  (if (empty? token-seq)
    false
    (let [token (first token-seq)]
      (pred token))))

; Check to see whether the next token matches the specified
; grammar symbol.
;
; Parameters:
;   token-seq - the input token sequence
;   symbol - a terminal symbol
;
; Returns:
;   true if there is at least one more token and its symbol
;   matches the specified terminal symbol
;
(defn next-token-is? [token-seq symbol]
  (next-token-matches? token-seq (fn [[lexeme tsym]] (= tsym symbol))))

; Check to see whether the next token's grammar symbol is
; contained in the specified collection.
;
; Parameters:
;   token-seq - the input token sequence
;   coll - a collection of grammar symbols
;
; Returns:
;   true if there is at least one more token and the
;   specified collection contains its grammar symbol
;
(defn next-token-in? [token-seq coll]
  (next-token-matches? token-seq (fn [[lexeme tsym]] (contains? coll tsym))))