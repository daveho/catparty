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
       (every? identity (map (fn [sfx]
                               (or (empty? sfx)
                                   (verify-token (str expected-lexeme sfx) expected-token-type))) suffixes))))

(defn int-suffixes []
  (for [first ["" "u" "U"]
        second ["" "l" "L" "ll" "LL"]]
    (str first second)))

(defn verify-dec-literal [lexeme]
  (verify-with-suffixes lexeme :dec_literal (int-suffixes)))

(defn verify-fp-literal [lexeme]
  (verify-with-suffixes lexeme :fp_literal ["f" "F" "l" "L"]))

(defn verify-hex-literal [lexeme]
  (verify-with-suffixes lexeme :hex_literal (int-suffixes)))

(deftest dec-literal-test
  (testing "decimal integer literals"
    (is (verify-dec-literal "4"))
    (is (verify-dec-literal "404"))
    (is (verify-dec-literal "01234567"))
    (is (verify-dec-literal "8989"))
    ))

(deftest hex-literal-test
  (testing "hex integer literals"
    (is (verify-hex-literal "0x0"))
    (is (verify-hex-literal "0x01234567"))
    (is (verify-hex-literal "0x89ABCDEF"))
    (is (verify-hex-literal "0X0"))
    (is (verify-hex-literal "0X01234567"))
    (is (verify-hex-literal "0X89ABCDEF"))
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
