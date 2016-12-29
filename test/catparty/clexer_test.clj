(ns catparty.clexer-test
  (:require [clojure.test :refer :all]
            [catparty.clexer :refer :all]
            [catparty.lexer :as l]))


(defn verify-token [s expected-lexeme expected-token-type]
  (let [lexer (create-from-string s)
        token-seq (l/token-sequence lexer)
        [lexeme token-type lnum cnum] (first token-seq)]
    (and (= token-type expected-token-type)
         (= lexeme expected-lexeme))))


(deftest char-literal-test
  (testing "character literals"
    (is (verify-token "'a'" "'a'" :char_literal))
    (is (verify-token "'\\''" "'\\''" :char_literal))
    ))
