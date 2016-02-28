(in-package :st-rexp-fa)

;;; define finite automata abstract data structure
(defparameter *fa* nil
  "The variable to store the finite automata")

(defvar *NFA-nodes* (make-hash-table)
  "The hash table to store all nodes in NFA,
   and the typical node structure looks like:
   '((input_1 . next_nodes_1) (input_2 . next_nodes_2))',
   node-name as key")

;;; macro definations
(defmacro cdr-assoc (key alist)
  `(cdr (assoc ,key ,alist :test 'equal)))

(defmacro with-gensyms ((&rest names) &body body)
  "define multiple unique names"
  `(let ,(loop for n in names collect
              `(,n (make-symbol ,(string n))))
     ,@body))

(defmacro keep-type ((type tree) &body body)
  "keep the same type for part-B of the tree"
  `(let ((part-B (cddr ,tree)))
     (if (cdr part-B)
         (setf part-B (cons ,type part-B))
         (if (atom (car part-B))
             (setf part-B (list :IDENTITY (car part-B)))
             (setf part-B (car part-B))))
     (setf (cddr ,tree) part-B)
     ,@body))

(defmacro alist-insert ((transfer start end) &body body)
  "macro utility to handle alist in `*NFA-nodes*' easily"
  `(let* ((alist (gethash ,start *NFA-nodes*))
          (old_kv_pair (assoc ,transfer alist :test 'equal)))
     (if old_kv_pair
         (if (atom (cdr old_kv_pair))
                                        ; only single adjacent node
             (setf (cdr-assoc ,transfer alist)
                   (list (cdr old_kv_pair) ,end))
                                        ; multiple adjacent nodes
             (setf (cdr-assoc ,transfer alist)
                   (cons ,end (cdr old_kv_pair))))
         (setf alist
               (cons (cons ,transfer ,end) alist)))
     (setf (gethash ,start *NFA-nodes*) alist)
     ,@body))

;;; supplementary functions
(defun insert-node (transfer start)
  "insert node and from start to the new node with `transfer'"
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((node (gensym)))
    (alist-insert (transfer start node)
      node)))

(defun connect-nodes (transfer start end)
  "connect end node from start node with given transfer"
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (alist-insert (transfer start end)
    end))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun print-hash-table (hash-table)
  (let ((hash-keys (hash-keys hash-table)))
    (loop for key in hash-keys do
         (format t "~a : ~a~%" key
                 (gethash key hash-table)))))

(defun reset-nfa ()
  (setf *NFA-nodes* (make-hash-table)))

;;; step I: regular expression to NFA
(defun regex2NFA (regex)
  "Convert regular expression to NFA"
  ;; cl-ppcre parse tree http://weitz.de/cl-ppcre/#create-scanner2
  ;; the mapping relationship: doc/map-regex-parse-tree.md
  (let ((parse-tree (cl-ppcre:parse-string regex)))
    (if (atom parse-tree)
        (print "not a parse tree")
        (parseTree parse-tree :START :END))))

(defun parseTree (tree start end)
  "Parse cl-ppcre parse tree recursively"
  (let ((type (car tree)))
    (case type
      (:REGISTER ; ignore
       (let ((new-tree (cadr tree)))
         (if (atom new-tree)
             (setf new-tree
                   (list :IDENTITY new-tree)))
         (parseTree new-tree start end)))
      (:IDENTITY ; identity
       (let ((new-node (insert-node (cadr tree)
                                    start)))
         (connect-nodes :EPSILON new-node end)))
      (:SEQUENCE ; AB type
       (keep-type (type tree)
         (let ((part-A (cadr tree))
               (part-B (cddr tree)))
           (if (atom part-A)
               (parseTree part-B
                          (insert-node part-A start)
                          end)
               (let ((new-node (gensym)))
                 (parseTree part-A start new-node)
                 (parseTree part-B new-node end))))))
      (:ALTERNATION ; A+B type
       (keep-type (type tree)
         (let ((part-A (cadr tree))
               (part-B (cddr tree)))
           (if (atom part-A)
               (connect-nodes :EPSILON
                              (insert-node part-A start)
                              end)
               (parseTree part-A start end))
           (parseTree part-B start end))))
      (:GREEDY-REPETITION ; A* or A+ type
       (let ((min-repeats (cadr tree))
             (max-repeats (caddr tree))
             (repeat-part (nth 3 tree)))
         (if (and (eq max-repeats NIL)
                  (or (eq min-repeats 1)
                      (eq min-repeats 0)))
             (let ((new-node (gensym)))
               (parseTree repeat-part start new-node)
               (connect-nodes :EPSILON new-node start)
               (connect-nodes :EPSILON new-node end)
               (if (eq min-repeats 0)
                   (connect-nodes :EPSILON start end)))
             (print "only support A* and A+ repeats"))))
      (otherwise
       (print "unrecognized symbol in parse tree")))))
