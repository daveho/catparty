(ns catparty.cparser
  (:require [catparty.parser :as parser]
            [catparty.lexer :as lexer]
            [catparty.clexer :as clexer]
            [catparty.prettyprint :as pp])
  )

(defn parse-declaration [token-seq]
  (exc/throw-exception "Declarations not implemented yet")
  )

(defn declaration-specifiers
  #{:kw_auto
    :kw_register
    :kw_static
    :kw_extern
    :kw_typedef})

(defn parse-declaration-specifiers [token-seq]
  
  )

(defn parse-opt-declaration-specifiers [token-seq]
  (if (or (empty? token-seq)
          (not (lexer/next-token-in? token-seq declaration-specifiers))
    ; epsilon production
    (do-production :opt_declaration_specifiers [] token-seq)
    
    ; there is at least one declaration specifier
    (do-production :opt_declaration_specifiers [parse-declaration-specifiers] token-seq)))

(defn parse-declarator [token-seq]
  
  )

(defn parse-opt-declaration-list [token-seq]
  
  )

(defn parse-compound-statement [token-seq]
  ; temporary placeholder
  (do-production :compound_statement [(parser/expect :lbrace) (parser/expect :rbrace)])
  )

(defn parse-function-definition [token-seq]
  (do-production :function_definition [parse-opt-declaration-specifiers
                                       parse-declarator
                                       parse-opt-declaration-list
                                       parse-compound-statement] token-seq)
  )

(defn parse-external-declaration [token-seq]
  ; Try parsing a function definition.
  ; If that fails, parse a declaration.
  (try
    (do-production :external_declaration [parse-function-definition] token-seq)
    (catch :default e
      (do-production :external_declaration [parse-declaration] token-seq))))

(defn parse-translation-unit [token-seq]
  ; Possibilities are:
  ;    translation-unit -> ^ external-declaration
  ;    translation-unit -> ^ external-declaration translation-unit
  (let [startpr (parser/initial-parse-result token-seq)
        ; Parse a single external declaration
        pr (parser/apply-production startpr [parse-external-declaration])
        remaining-tokens (:tokens pr)]
    (if (empty? remaining-tokens)
      ; At EOF, end the production here
      (parser/complete-production :translation-unit pr)
      ; Not at EOF, continue recursively
      (parser/complete-production :translation-unit
                                  (parser/apply-production pr [parse-translation-unit]))
      )
  ))


(defn parse [token-seq]
  (:node (parse-translation-unit token-seq)))

;; Just for testing...

(def testprog "1 2\n3 4")
(def t (parse (lexer/token-sequence (clexer/create-from-string testprog))))

