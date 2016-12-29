(ns catparty.clexer-test
  (:require [clojure.test :refer :all]
            [catparty.clexer :refer :all]
            [catparty.lexer :as l]))

(defn lexit [s]
  (let [lexer (create-from-string s)]
    (l/token-sequence lexer)))

(defn verify-token [expected-lexeme expected-token-type]
  (let [lexer (create-from-string expected-lexeme)
        token-seq (l/token-sequence lexer)
        [lexeme token-type lnum cnum] (first token-seq)]
    (and (= token-type expected-token-type)
         (= lexeme expected-lexeme))))

;; Verify that a lexeme scans as a specified token type,
;; both unmodified and with the addition of the specified
;; suffixes.
(defn verify-with-suffixes [expected-lexeme expected-token-type suffixes]
  (and (verify-token expected-lexeme expected-token-type)
       (every? identity (map (fn [sfx] (verify-token (str expected-lexeme sfx) expected-token-type))
                             suffixes))
       
       ))

(defn dec-suffixes []
  (for [first ["" "u" "U"]
        second ["" "l" "L" "ll" "LL"]]
    (str first second)))

(defn verify-dec-literal [lexeme]
  (verify-with-suffixes lexeme :dec_literal (dec-suffixes)))

(defn verify-fp-literal [lexeme]
  (verify-with-suffixes lexeme :fp_literal ["f" "F" "l" "L"]))


;; Verify that a lexeme is scanned as a decimal integer literal,
;; trying all of the legal suffixes.

(deftest dec-literal-test
  (testing "decimal integer literals"
    (is (verify-dec-literal "4"))
    ))

(deftest char-literal-test
  (testing "character literals"
    (is (verify-token "'a'" :char_literal))
    (is (verify-token "'\\''" :char_literal))
    ))


(deftest fp-literal-test
  (testing "floating point literals"
    (is (verify-fp-literal "2.3"))
    (is (verify-fp-literal "2."))
    (is (verify-fp-literal ".3"))
    (is (verify-fp-literal "2.3e5"))
    (is (verify-fp-literal "2.3e+5"))
    (is (verify-fp-literal "2.3e-5"))
    ))
