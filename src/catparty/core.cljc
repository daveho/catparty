; -*- mode: clojure -*-

;; Cat Party - C parser in Clojure/ClojureScript
;; Copyright (c) 2016-2017, David H. Hovemeyer <david.hovemeyer@gmail.com>
;;
;; This is free software distributed under the GNU Public License, version 3,
;; or any later version.  See COPYING.txt for details.

(ns catparty.core
  (:require [clojure.browser.repl :as repl]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

(enable-console-print!)

(println "Hello, world! And now there's more!")

(defn foo [a b]
  (+ a b))
