(ns catparty.prettyprint)

; ------------------------------------------------------------
; Pretty printing the parse tree
; ------------------------------------------------------------

(defn node-to-string [node]
  (if (string? (:value node))
    (str (:symbol node) "[\"" (:value node) "\"]")
    (str (:symbol node))))

(defrecord PPItem [node children visited])

(defn node-to-item [node]
  (PPItem. node
           (if (string? (:value node)) '() (:value node))
           false))

(defn visit-item [item]
  (PPItem. (:node item) (:children item) true))

(defn remove-first-child [item]
  (PPItem. (:node item) (rest (:children item)) true))

(defn has-remaining-children? [item]
  (not (empty? (:children item))))

(defrecord PP [stack])

;(defn print-stack [stack]
;  (loop [items (reverse stack)]
;    (if (empty? items)
;      (println "")
;      (let [item (first items)
;            node (:node item)]
;        (print (str (:symbol node) "," (count (:children item)) "," (:visited item)))
;        (recur (rest items))))))

(defn print-item [pp node]
  ;(print-stack (:stack pp))
  (loop [items (reverse (:stack pp))]
    (if (empty? items)
      (println (node-to-string node))
      (let [item (first items)]
        (do
          (if (empty? (rest items))
            (print "+--")
            (if (has-remaining-children? item)
              (do
                ;(print (str ((:symbol (:node item))) " has more children"))
                (print "|  "))
              (print "   ")))
          (recur (rest items)))))))

(defn create-pretty-printer [node]
  (PP. (list (node-to-item node))))

(defn is-top-item-visited? [pp]
  (let [top-item (first (:stack pp))]
    (:visited top-item)))

(defn visit-top-item [pp]
  (let [top-item (first (:stack pp))]
    (PP. (cons (visit-item top-item) (rest (:stack pp))))))

(defn schedule-child-of-top-item [pp]
  (let [top-item (first (:stack pp))
        remaining-items (rest (:stack pp))
        first-child-node (first (:children top-item))
        first-child-item (node-to-item first-child-node)
        remaining-children-item (remove-first-child top-item)]
    (PP. (cons first-child-item (cons remaining-children-item remaining-items)))))

(defn remove-top-item [pp]
  (PP. (rest (:stack pp))))

(defn pretty-print [node]
;  (let [pp (create-pretty-printer node)]
;    (print-item pp (:node (first (:stack pp))))))
  (loop [pp (create-pretty-printer node)]
    (if (not (empty? (:stack pp)))
      (let [top-item (first (:stack pp))]
        ;(println "blart!")
        (if (not (:visited top-item))
          ; Top item hasn't been visited yet: print it, and recur with
          ; the top item marked as visited
          (do
            (print-item (remove-top-item pp) (:node top-item))
            (recur (visit-top-item pp)))
          ; Top item has been visited.
          (if (has-remaining-children? top-item)
            ; The top item has remaining children: schedule the first
            ; child for visitation.
            (recur (schedule-child-of-top-item pp))
            ; The top item has no remaining children: recur with the stack popped.
            (recur (remove-top-item pp))))))))
