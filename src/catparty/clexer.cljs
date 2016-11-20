(ns catparty.clexer)

;; C lexical analyzer

(def c-keywords
  ["auto"
   "break"
   "case"
   "default"
   ])

(def c-keyword-patterns
  (mapv (fn [kw] [(re-pattern (str "^" kw)) (keyword (str "kw_" kw))]) c-keywords))

(def all-patterns
  (concat c-keyword-patterns))

;; Note: would need to be implemented differently in Clojure
;; (as opposed to ClojureScript.)
(defn throw-lexer-error [msg]
  (throw (js/Error. msg)))

(defn match-token
  "Attempt to match a token at the beginning of given string,
  returning a [lexeme token-type] vector if successful.  Throws
  an error if the input does not match any token type."
  [s]
  (loop [pats all-patterns]
    (if (empty? pats)
      (throw-lexer-error (str "Illegal input: " s))
      (let [[re sym] (first pats)
            m (re-find re s)]
        (if m
          [m sym] ; Match, return the token
          (recur (rest pats)))))))
