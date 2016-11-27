(ns catparty.cparser
  (:require [catparty.parser :as parser])
  )

(defn parse-external-declaration [token-seq]
  ; FIXME: this is just a placeholder for now
  (parser/do-production :external-declaration [(parser/expect :int_literal)] token-seq)
  )

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

