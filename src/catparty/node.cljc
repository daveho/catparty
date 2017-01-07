; -*- mode: clojure -*-

;; Cat Party - C parser in Clojure/ClojureScript
;; Copyright (c) 2016-2017, David H. Hovemeyer <david.hovemeyer@gmail.com>
;;
;; This is free software distributed under the GNU Public License, version 3,
;; or any later version.  See COPYING.txt for details.

(ns catparty.node
  (:require [catparty.exc :as exc]))

; Record for parse tree, AST, and augmented AST nodes.
;
; symbol indicates the (terminal or nonterminal) symbol.
;
; value is the "value" of the node, which for terminal nodes
; is the lexeme of the token, and for nonterminal nodes
; is the list of child nodes.
;
; props is a map containing property values.
;
(defrecord Node [symbol value props])

; Make a node without any properties.
;
; Parameters:
;   symbol - the symbol (keyword value) used to label the node
;   value  - the value of the node: for parent nodes, it should
;            be a sequence (list or vector) containing the child
;            nodes
;
; Returns: the node
;
(defn make-node [symbol value]
  (Node. symbol value {}))

; Make a node with specified properties.
;
; Parameters:
;   symbol - the symbol (keyword value) used to label the node
;   value  - the value of the node: for parent nodes, it should
;            be a sequence (list or vector) containing the child
;            nodes
;   props  - map containing node properties
;
; Returns: the node
;
(defn make-node-with-props [symbol value props]
  (Node. symbol value props))

;; Add a child to given Node.
;;
;; Parameters:
;;   node - a Node
;;   child - a child Node to add
;;
;; Returns:
;;   a new Node which is the same as the original one, but with
;;   the child added
;;
(defn add-child [node child]
  (Node. (:symbol node) (conj (:value node) child) (:props node)))


;; Replace the children of given node.
;;
;; Parameters:
;;   node - a node
;;   new-children - a new sequence of children
;;
;; Returns:
;;   node with same data as original node, but with the
;;   children replaced
;;
(defn replace-children [node new-children]
  (assoc node :value new-children))


;; Relabel a node with a different symbol.
;;
;; Parameters:
;;   node - a Node
;;   new-symbol - the new symbol
;;
;; Returns:
;;   a Node identical to the original Node, except with
;;   the new symbol
;;
(defn relabel [node symbol]
  (assoc node :symbol symbol))


; Add properties to a node, producing an equivalent node
; (with the specified new properties, but retaining any existing
; properties that don't conflict with the new properties.)
;
; Parameters:
;   node - a node
;   props - new properties
;
; Return: a new node with updated properties
;
(defn add-props [node props]
  (make-node-with-props (:symbol node) (:value node) (merge (:props node) props)))

; Get the children of given parent node.
;
; Parameters:
;   node - a node
;
; Returns: a sequence (list or vector) containing the children;
;   an exception is thrown if this is not a parent node
;
(defn children [node]
  (if (not (sequential? (:value node)))
    (exc/throw-exception (str "Attempt to get children of terminal node " (:symbol node)))
    (:value node)))

; Return how many children the given node has.
;
; Parameters:
;   node - a node
;
; Returns: a count of how many children the node has
;
(defn num-children [node]
  (if (not (sequential? (:value node)))
    0
    (count (:value node))))


;; Check whether specified node has any children.
;;
;; Parameters:
;;   node - a node
;;
;; Returns:
;;   true if the node has at least one child, false otherwise
;;
(defn has-children? [node]
  (> (num-children node) 0))


; Get the specified child of given node.
;
; Parameters:
;   node - a node
;   n    - the index specifying which child to get (0 for first child)
;
; Returns: the child
(defn get-child [node n]
  (nth (children node) n))

; Determine whether the given node has the specified property.
;
; Parameters:
;   node - a node
;   prop - a property, e.g. :regnum, :nlocals
;
; Returns: true if the node has the property, false if it does not
;
(defn has-prop? [node prop]
  (contains? (:props node) prop))

; Get the value of a specified property.  For example, if the node
; n has the :regnum property specified, then
;
;    (get-prop n :regnum)
;
; would return the value of n's :regnum property.
;
; Parameters:
;    node - a node
;    prop - a property, e.g., :regnum, :nlocals
;
; Returns: the value of the property, or nil if the node does
; not have the property
;
(defn get-prop [node prop]
  (get (:props node) prop))


;; Find the node specified by given path.
;; If no such node exists, return false.
;; Otherwise, return nil.
(defn find-node [start path]
  (loop [n start
         p path]
    (if (empty? p)
      ; Found the node named by the path
      n
      (let [nchildren (num-children n)
            index (first p)]
        (if (>= index nchildren)
          ; the node named by the path doesn't exist
          nil
          ; continue recursively in specified child
          (recur (get-child n index) (rest p)))))))


;; Find the node specified by given path.
;; If no such node exists, return false.
;; Otherwise, return the result of applying the specified
;; predicate function to the node.
;;
;; Parameters:
;;   start - the starting node
;;   path - the path (sequence of child index values describing
;;          a path to a descendent of the start node)
;;   pred - predicate function to be applied to the node named
;;          by the path
;;
;; Returns:
;;   false if the node named by the path doesn't exist,
;;   otherwise the value returned by invoking the predicate
;;   function on the node named by the path
;;
(defn check-node [start path pred]
  (let [n (find-node start path)]
    (if (nil? n)
      false
      (pred n))))


;; Create a "filtered" view of a tree rooted at specified node.
;; Only the children selected by the specified predicate function will
;; be visible in the tree.
;;
;; Parameters:
;;   n - a Node
;;   pred - a Node predicate
;;
;; Returns:
;;   a view of the tree rooted at n in which nodes not matched by the
;;   predicate are not visible; note that n itself cannot be made invisible
;;
(defn filter-tree [n pred]
  (if (not (has-children? n))
    ; Do nothing 
    n
    ; Replace children with filtered sequence (just the children
    ; that match the predicate), in which each child is
    ; recursively filtered
    (let [children (map #(filter-tree % pred) (filter pred (:value n)))]
      (replace-children n children))))


;; Recursively search (preorder) tree rooted at given node
;; for a node matching given predicate.
;;
;; Parameters:
;;   n - a Node
;;   pred - a Node predicate
;;
;; Returns:
;;   a Node matching the predicate, or nil if there is no such node
;;
(defn search [n pred]
  (cond
    (pred n) n
    (not (has-children? n)) nil
    ; FIXME: this seems inelegant
    :else (loop [kids (children n)]
            (if (empty? kids)
              nil
              (let [kid (first kids)
                    result (search kid pred)]
                (if result
                  result
                  (recur (rest kids))))))))
