(ns catparty.exc)

;; Cat Party - C parser in Clojure/ClojureScript
;; Copyright (c) 2016-2017, David H. Hovemeyer <david.hovemeyer@gmail.com>
;;
;; This is free software distributed under the GNU Public License, version 3,
;; or any later version.  See COPYING.txt for details.

(defn throw-exception
  "Generic function for throwing an exception (for Clojure/ClojureScript
  compatibility)"
  [msg]
  (throw (js/Error. msg)))
