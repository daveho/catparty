(ns minilang.node)

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
    (throw (RuntimeException. (str "Attempt to get children of terminal node " (:symbol node))))
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
