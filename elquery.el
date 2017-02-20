;;; elquery.el --- The HTML library for elisp. -*- lexical-binding: t -*-

;; Copyright 2017 Adam Niederer

;; Author: Adam Niederer
;; Maintainer: Adam Niederer
;; Created: 19 Feb 2017

;; Keywords: html hypermedia tools webscale
;; Homepage: https://github.com/AdamNiederer/elquery
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (s "1.11.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(require 's)
(require 'cl-lib)
(require 'subr-x)

;; Functions to eventually break out into a seperate tree library
(defun elq-tree-remove-if (pred tree)
  "Remove all elements from TREE if they satisfy PRED. Preserves the structure
 and order of the tree."
  (if (not (listp tree)) tree
    (thread-last tree
      (cl-remove-if (lambda (e) (and (not (listp e)) (funcall pred e))))
      (mapcar (lambda (e) (if (and (listp e) (not (elq--dotp e)))
                              (elq-tree-remove-if pred e)
                            e))))))

(defun elq-tree-remove-if-not (pred tree)
  "Remove all elements from TREE if they do not satisfy PRED. Preserves the
structure and order of the tree."
  (elq-tree-remove-if (lambda (e) (not (funcall pred e))) tree))

(defun elq-tree-mapcar (fn tree)
  "Apply FN to all elements in TREE"
  (if (not (listp tree)) tree
    (mapcar (lambda (e) (if (and (listp e) (not (elq--dotp e)))
                            (elq-tree-mapcar fn e)
                          (funcall fn e)))
            tree)))

(defun elq-tree-reduce (fn tree)
  "Perform an in-order reduction of TREE with FN. Equivalent to a reduction on
a flattened tree"
  (if (not (listp tree)) nil
    (cl-reduce (lambda (a b) (if (and (listp b) (not (elq--dotp b)))
                                 (funcall fn a (elq-tree-reduce fn b))
                               (funcall fn a b)))
               tree))) ; TODO: Dash is way faster at this. Add a dependency?

(defun elq-tree-flatten (tree)
  "Flatten the tree, removing all list nesting and leaving a list of only atomic
elements. This does not preserve the order of the elements."
  ;; TODO: elq-tree-flatten-inorder, elq-tree-flatten-postorder, elq-tree-flatten-preorder
  (let ((ret nil))
    (dolist (el tree ret)
      (if (or (not (listp el) (elq--alistp el)))
          (setq ret (cons el ret))
        (setq ret (append (elq-tree-flatten el) ret))))))

(defun elq-tree-flatten-until (pred tree)
  "Flatten the tree, but treat elements matching PRED as atomic elements, not
preserving order"
  (let ((ret nil))
    (dolist (el tree ret)
      (if (or (not (listp el)) (elq--alistp el) (funcall pred el))
          (setq ret (cons el ret))
        (setq ret (append (elq-tree-flatten-until pred el) ret))))))

;; Functions to eventually upstream into Dash
(defun elq--alistp (list)
  "Return whether LIST is an alist"
  (if (or (not (listp list)) (elq--dotp list)) nil
    (cl-reduce (lambda (a b) (and a (elq--dotp b))) (cons t list))))

(defun elq--alist-to-plist (list)
  "Convert alist LIST to a plist, preserving all key-value relationships."
  (cl-reduce (lambda (plist dot) (append plist (list (elq--to-kw (car dot)) (cdr dot))))
             (cons '() list)))

(defun elq--sym-to-kw (sym)
  "Convert symbol OBJ to a keyword"
  (intern (concat ":" (prin1-to-string sym))))

(defun elq--kw-to-string (kw)
  "Convert keyword OBJ to a string"
  (substring (prin1-to-string kw) 1))

(defun elq--kw-to-sym (kw)
  "Convert keyword OBJ to a symbol"
  (make-symbol (substring (prin1-to-string kw) 1)))

(defun elq--to-kw (obj)
  "Convert OBJ to a keyword"
  (if (keywordp obj) obj
    (elq--sym-to-kw (if (symbolp obj) obj
                      (make-symbol (if (stringp obj) obj
                                     (prin1-to-string obj)))))))
(defun elq--dotp (obj)
  "Return whether OBJ is a dotted cons cell (e.g. '(a . b))"
  (and (consp obj) (cdr obj) (not (consp (cdr obj)))))

(defun elq--plist-set! (list kw val)
  ;; TODO: Does plist-put do everything this function does?
  (let ((ptr list))
    (while (and (cdr ptr) (not (equal (car ptr) kw)))
      (setq ptr (cdr ptr)))
    (if (cdr ptr)
        (setcar (cdr ptr) val)
      (setcdr ptr (list kw))
      (setcdr (cdr ptr) (list val)))))

;; (defun elq--plistp (obj &optional key-fn)
;;   "Heuristically determine if OBJ is a plist. Keys will be tested against
;; key-fn if provided; otherwise, this function will simply check if the list is
;; of even length and if keys are of the same type"
;;   (if key-fn
;;       (elq--reduce-nth )
;;       ))

(defun elq--plist-remove-if (list pred)
  "Return a copy of LIST with all keys satisfying PRED removed."
  ;; TODO: Make this not break if a value satisfies PRED
  (let ((ignore-next nil))
    (cl-remove-if (lambda (e)
                    (cond
                     ((funcall pred e) (progn (setq ignore-next t) t))
                     (ignore-next (progn (setq ignore-next nil) t))
                     (t nil)))
                  list)))

(defun elq--plist-keys (list)
  "Return a list of keys from plist LIST"
  (let ((i 0))
    (cl-remove-if (lambda (_) (prog1 (equal (% i 2) 1)
                                (setq i (1+ i))))
                  list)))

(defun elq--plist-equal (a b &optional keys)
  "Return whether plists A and B are equal in content. If KEYS is supplied, only
test keys from that list."
  ;; TODO: Make this not O(n^2)
  (let ((keys (or keys (cl-union (elq--plist-keys a) (elq--plist-keys b))))
        (iseq t))
    (dolist (key keys iseq)
      (when (not (equal (plist-get a key) (plist-get b key)))
        (message "%s failed |%s %s| ||%s %s|| |||%s|||" key (plist-get a key) (plist-get b key) a b keys)
        (setq iseq nil)))))

(defun elq--subset? (sub set)
  "Return whether the set of the elements of SUB is a subset of the elements of
 SET"
  ;; This is currently O(rn). We can get it down to nlogn by sorting and
  ;; linearly searching, but I'm not sure if there's a clean solution for this
  ;; in elisp.
  (cl-reduce (lambda (a b) (and a (member b set))) (cons t sub)))

(defun elq--set-equal (a b)
  "Return whether A and B contain the same members, regardless of their ordering"
  (and (elq--subset? a b) (elq--subset? b a)))

;; Functions to eventually upstream into s
(defun elq--whitespace? (s)
  "Return whether S consists solely of whitespace characters"
  (and (stringp s) (s-matches-p "^[[:space:]]*$" s)))

;; Predicates specific to elquery
(defun elq-elp (obj)
  "Return whether OBJ is a DOM element"
  (plist-get obj :el))

;; Accessor Aliases on DOM nodes
(defun elq-props (node)
  "Return a list of NODE's properties (id, class, data*, etc)"
  (plist-get node :props))

(defun elq-children (node)
  "Return a list of the children of NODE"
  (plist-get node :children))

(defun elq-next-children (node)
  "Return a list of children of NODE with their children removed"
  (mapcar (lambda (e) (append '(:children nil) e)) (elq-children node)))

(defun elq-siblings (node)
  "Return a list of NODE's siblings, including NODE."
  (plist-get (plist-get node :parent) :children))

(defun elq-prop (node prop &optional val)
  "Return the value of PROP in NODE. If VAL is supplied, destructively set PROP
to VAL."
  (let ((props (elq-props node)))
    (when val
      (elq--plist-set! props (elq--to-kw prop) val))
    (plist-get props (elq--to-kw prop))))

(defun elq-rm-prop (node prop)
  "Destructively remove PROP from NODE"
  (elq--plist-set! node :props (cl-remove-if (lambda (p) (equal (car p) prop)) props)))

(defun elq-parent (node)
  "Return the parent of NODE"
  (plist-get node :parent))

(defun elq-el (node)
  "Return NODE's element name (e.g. body, div, span)"
  (plist-get node :el))

(defun elq-text (node)
  "Return the text content of NODE. If there are multiple text nodes in NODE
(e.g. <h1>some text<span></span>more text</h1>), return the concatenation of
these text nodes"
  (plist-get node :text))

(defun elq--new-node (el parent &optional children text props)
  ;; TODO: Will we actually ever need this?
  (append `(:el ,el :text ,(or text "")) :parent ,parent :children ,children) props)

(defun elq-classes (node)
  "Return a list of NODE's classes"
  (let ((classes (elq-prop node :class)))
    (if (listp classes) classes
      (s-split " " classes))))

(defun elq-class? (node class)
  "Return whether NODE has the class CLASS"
  (if (member class (elq-classes node)) t nil))

(defun elq-id (node)
  "Return the id of NODE"
  (elq-prop node :id))

(defun elq-data (node key &optional val)
  "Return the value of NODE's data-KEY property. If VAL is supplied,
destructively set NODE's data-KEY property to VAL"
  (if val
      (elq--set-prop (s-concat "data-" key) val)
    (elq-prop node (s-concat "data-" key))))

(defun elq-read-file (file)
  "Returns the AST of the HTML file as a plist"
  (with-temp-buffer
    (insert-file-contents file)
    (let ((tree (libxml-parse-html-region (point-min) (point-max))))
      (thread-last tree
        (elq-tree-remove-if 'elq--whitespace?)
        (elq--parse-libxml-tree nil)))))

(defun elq--parse-libxml-tree (parent tree)
  "Convert libxml's alist-heavy and position-dependant format to a plist format,
remove some useless whitespace nodes, and recursively set nodes' parents via
pointer bashing."
  (if (stringp tree)
      `(:el nil :text ,tree :children nil :parent ,parent)
    (let ((self (append (list :el (prin1-to-string (car tree)))
                        (list :text (cl-reduce (lambda (a b)
                                                 (if (stringp b) (concat a b) a))
                                               (cons "" (cdr (cdr tree)))))
                        (list :props (elq--alist-to-plist (cl-second tree)))
                        (list :children nil)
                        (list :parent nil))))
      (elq--plist-set! self :parent parent) ; We can't do functional stuff here because we need a circular list, which requires messing with pointers
      (elq--plist-set! self :children (mapcar (lambda (e)
                                                (elq--parse-libxml-tree self e))
                                              (cdr (cdr tree))))
      self)))

(defun elq--intersects? (query tree)
  (and (if (not (elq-el query)) t
         (equal (elq-el tree) (elq-el query)))
       (if (not (elq-classes query)) t
         (elq--subset? (elq-classes query) (elq-classes tree)))
       (let ((keys (cl-remove-if (lambda (e) (member e '(:class)))
                                 (elq--plist-keys (elq-props query)))))
         (if (not keys) t
           (elq--plist-equal (elq-props tree)
                             (elq-props query)
                             keys)))))

(defconst elq--el-re "^[A-Za-z0-9\\-]+")
(defconst elq--classes-re "\\.\\([a-zA-Z0-9]+\\)")
(defconst elq--id-re "#\\([a-zA-Z0-9]+\\)")
(defconst elq--attr-re "\\[\\([A-z\\-]+\\)=\\(.+?\\)\\]")
(defconst elq--heirarchy-re (concat "^\\([^[:space:]]+\\)" ; Head of heirarchy tree
                                    "[[:space:]]*\\([>~+]?\\)[[:space:]]*" ; Relationship operator
                                    "\\(.+\\)?")) ; Rest of tree (recursively parsed)

(defun elq--parse-intersection (string)
  "Return a plist representing a single intersection in a query, like
span#kek.bur[foo=bar]"
  (let* ((el (car (s-match elq--el-re string)))
         (attrs (elq--alist-to-plist
                 (mapcar (lambda (match) (apply 'cons (cdr match)))
                         (s-match-strings-all elq--attr-re string))))
         (id (cl-second (s-match elq--id-re string)))
         (classes (mapcar 'cl-second (s-match-strings-all elq--classes-re string))))
    (list :el el :props (append (if id `(:id ,id))
                                (if classes `(:class ,classes))
                                attrs))))

(defun elq--rel-kw (string)
  "Return a readable keyword for the relationship operator STRING"
  (cond
   ((equal string ">") :next-child)
   ((equal string "+") :next-sibling)
   ((equal string "~") :sibling)
   (t :child)))

(defun elq--parse-heirarchy (string)
  "Return a plist representing a heirarchical structure in a query, For example,
#foo .bar > #bur[name=baz] returns
(:el nil :props (:id \"foo\") :rel :child :children
     (:el nil :props (:class \"bar\") :rel :next-child :children
          (:el nil :props (:id \"bur\" :name \"baz\") :rel :child :children
               nil)))"
  (let* ((match (s-match elq--heirarchy-re (s-trim string)))
         (head (cl-second match))
         (rel (cl-third match))
         (rest (cl-fourth match)))
    (append (elq--parse-intersection head)
            `(:rel ,(elq--rel-kw rel)
              :children ,(if rest (elq--parse-heirarchy rest) nil)))))

(defun elq--parse-union (string)
  "Return a list of plists representing the query STRING"
  (mapcar 'elq--parse-heirarchy (s-split ", *" string)))

(defun elq--$-next (query tree)
  "Return a subtree of TREE corresponding to the relationship operator :rel
in QUERY"
  (cond
   ((equal (plist-get query :rel) :next-child) (elq-children tree))
   ((equal (plist-get query :rel) :next-sibling) (error "TODO"))
   ;; TODO: Does returning the siblings INCLUDING the element cause
   ;; issues with selectors like el-type ~ el-type?
   ((equal (plist-get query :rel) :sibling) (elq-siblings tree))
   ((equal (plist-get query :rel) :child) (elq-children tree))))

(defun elq--$-recurse? (query)
  "Return whether recursion until finding a matching element is allowed for
the relationship operator :rel in QUERY"
  (equal (plist-get query :rel) :child))

(defun elq--$ (query tree can-recurse)
  "Return a subtree of TREE matching QUERY, or nil if no subtree is found. If
CAN-RECURSE is set, continue down the tree until a matching element is found."
  (message "TREE: %s\nQUERY: %s\nMATCH: %s\nRECURSE: %s" tree query (elq--intersects? query tree) can-recurse)
  (cond
   ;; If we're out of stuff to search, we can't do anything else
   ((equal tree nil) (message "Leaf did not match") nil)
   ;; No children in the query, no children in the tree, and a match in the tree
   ;; means we can return the leaf
   ((and (elq--intersects? query tree) (not (elq-children query)) (not (elq-children tree)))
    (progn (message "Leaf matched; return it")
           tree))
   ;; A match with children remaining in the query to find means we have to
   ;; recurse according to the query's heirarchy relationship
   ((and (elq--intersects? query tree) (elq-children query))
    (progn
      (message "Match with remaining heirarchy requirements: Return recursed match")
      (cl-remove-if-not 'identity (mapcar (lambda (child)
                                            (elq--$ (elq-children query) child
                                                    (elq--$-recurse? query)))
                                          (elq--$-next query tree)))))
   ;; A match without children in the query in the query will return the tree,
   ;; but we must still recurse to find any matching children in tree if we
   ;; aren't looking for siblings or next-children
   ((and can-recurse (elq--intersects? query tree) (not (elq-children query)))
    (progn
      (message "Match with recurse flag: Return subtree plus recursive matches")
      (append (list tree) (cl-remove-if-not 'identity (mapcar (lambda (child)
                                                                (elq--$ query child t))
                                                              (elq-children tree))))))
   ;; No match and a recurse flag means we can continue down the tree and see if
   ;; we get any matches. If we do, collect them in a list
   (can-recurse
    (progn
      (message "No match with recurse flag: recurse again")
      (cl-remove-if-not 'identity (mapcar (lambda (child)
                                            (elq--$ query child t))
                                          (elq-children tree)))))
   ;; No match and no allowed recursion means we can't do anything else
   (t nil)))

(defun elq-$ (query-string tree)
  "Return a list of elements in the subtree of TREE mathing QUERY-STRING."
  (let ((queries (elq--parse-union query-string)))
    (elq-tree-flatten-until 'elq-elp
                            (cl-remove-if-not 'identity
                                              (mapcar (lambda (query)
                                                        (elq--$ query tree t))
                                                      queries)))))

(defun elq--write-props (node)
  "Return a string representing the properties of NODE"
  (let ((props (elq-props node)))
    (if (not props) ""
      (s-concat " " (s-join " " (mapcar (lambda (key)
                                          (format "%s=\"%s\""
                                                  (elq--kw-to-string key)
                                                  (plist-get props key)))
                                        (elq--plist-keys props)))))))

(defun elq--indent-insert (string depth whitespace?)
  "Insert the proper amount of indentation, based on `sgml-basic-offset' and
DEPTH, then STRING, then a newline. If WHITESPACE? is nil, do not insert any
indentation or newline."
  (insert (if whitespace? (s-repeat (* depth sgml-basic-offset) " ") "")
          string
          (if whitespace? "\n" "")))

(defun elq--write (tree depth &optional whitespace?)
  "A recursive subroutine for `elq-write'. Inserts the HTML string
representation of TREE into the current buffer."
  ;; TODO: Close singleton elements with no children (<input/> and friends)
  (if (not (elq-el tree))
      (elq--indent-insert (elq-text tree) depth whitespace?)
    (elq--indent-insert (format "<%s%s>" (elq-el tree) (elq--write-props tree)) depth whitespace?)
    (dolist (child (elq-children tree))
      (elq--write child (1+ depth) whitespace?))
    (elq--indent-insert (format "</%s>" (elq-el tree)) depth whitespace?)))

(defun elq-write (tree &optional whitespace?)
  "Return an html string representing TREE. If WHITESPACE? is non-nil, insert
indentation and newlines according to `sgml-basic-offset'"
  (with-temp-buffer
    (elq--write tree 0 whitespace?)
    (buffer-string)))

(provide 'elquery)
;;; elquery.el ends here
