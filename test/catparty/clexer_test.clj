(ns catparty.clexer-test
  (:require [clojure.test :refer :all]
            [catparty.clexer :refer :all]
            [catparty.lexer :as l]))


(defn verify-token [expected-lexeme expected-token-type]
  (let [lexer (create-from-string expected-lexeme)
        token-seq (l/token-sequence lexer)
        [lexeme token-type lnum cnum] (first token-seq)]
    (and (= token-type expected-token-type)
         (= lexeme expected-lexeme))))


(deftest char-literal-test
  (testing "character literals"
    (is (verify-token "'a'" :char_literal))
    (is (verify-token "'\\''" :char_literal))
    ))

;; Verify that a lexeme is scanned as a floating point literal,
;; trying all of the legal suffixes (as well as no suffix).
(defn verify-fp-literal [lexeme]
  (and (verify-token lexeme :fp_literal)
       (every? identity (map (fn [sfx] (verify-token (str lexeme sfx) :fp_literal)) ["f" "F" "l" "L"]))))

(deftest fp-literal-test
  (testing "floating point literals"
    (is (verify-fp-literal "2.3"))
    (is (verify-fp-literal "2."))
    (is (verify-fp-literal ".3"))
    (is (verify-fp-literal "2.3e5"))
    (is (verify-fp-literal "2.3e+5"))
    (is (verify-fp-literal "2.3e-5"))
    ))
