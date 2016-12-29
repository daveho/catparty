(ns catparty.exc)

(defn throw-exception
  "Generic function for throwing an exception (for Clojure/ClojureScript
  compatibility)"
  [msg]
  (throw (RuntimeException. msg)))
