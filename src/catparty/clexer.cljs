(ns catparty.clexer)

;; C lexical analyzer

(defrecord Lexer [lineseq line tok])

(def c-keywords
  ["auto"
   "break"
   "case"
   "char"
   "const"
   "continue"
   "default"
   "do"
   "double"
   "else"
   "enum"
   "extern"
   "float"
   "for"
   "goto"
   "if"
   "int"
   "long"
   "register"
   "return"
   "short"
   "signed"
   "sizeof"
   "static"
   "struct"
   "switch"
   "typedef"
   "union"
   "unsigned"
   "void"
   "volatile"
   "while"
   ])

(def c-keyword-patterns
  (mapv (fn [kw] [(re-pattern (str "^" kw)) (keyword (str "kw_" kw))]) c-keywords))

(def all-patterns
  (concat c-keyword-patterns))

;; Note: would need to be implemented differently in Clojure
;; (as opposed to ClojureScript.)
(defn throw-lexer-error [msg]
  (throw (js/Error. msg)))

; Attempt to ensure that the lexer has a line available
(defn fill-line [lexer]
  (cond
    ; If there is already a line, there's nothing to do
    (not (empty? (:line lexer))) lexer
    ; If there are no more input lines, then set line to nil (to signal EOF)
    (empty? (:lineseq lexer)) (assoc lexer :line nil)
    ; Get the next line
    :else (Lexer. (rest (:lineseq lexer)) (first (:lineseq lexer)) (:tok lexer))))

(defn recognize-token [lexer]
  ; Trim leading whitespace
  (let [line (clojure.string/triml (:line lexer))]
    ; Attempt to match each token pattern in sequence
    (loop [patterns all-patterns]
      ; If there are no more patterns, then the input contains an illegal token
      (if (empty? patterns)
        (throw-lexer-error (str "Illegal token at " line))
        (let [[token-regexp token-type] (first patterns)
              match (re-find token-regexp line)]
          (if match
            ; Current pattern is a match: update lexer's line and token
            (let [match-text (if (vector? match) (get match 0) match)
                  rest-of-line (subs line (count match-text))]
              (assoc lexer :line rest-of-line :tok [match-text token-type]))
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
  "Create a lexer from a sequence of lines."
  [lines]
  (fill-token (Lexer. lines "" nil)))

(defn create-from-string
  "Create a lexer from an input string."
  [s]
  (let [lines (clojure.string/split s #"\n")]
    (create-from-lines lines)))

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
