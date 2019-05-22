;;;; analyzer.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-analyzer)

;;; Macros

;; This macro is commented-out because it may still be
;; useful when errors are found.
;; (defmacro debug-log (warning-msg arguments)
;;   `(progn
;;      (format t ,(if (listp arguments)
;; 		    "~a (L~aC~a): ~{~a ~}~%"
;; 		    "~a (L~aC~a): ~a~%")
;; 	     ,warning-msg
;; 	     (line-number *the-head*)
;; 	     (column-number *the-head*)
;; 	     ,arguments)))



;;; Terminal rules

(defun match-builtin (rule)
  "Matches a specific built-in RULE. RULE can be one of
:identifier, :string-constant or :integer-constant."
  (head-checkpoint (*the-head*)
    (let ((match-result (head-match *the-head* rule)))
      (unless (null match-result)
	(list rule match-result)))))

(defun match-group (rule-list)
  "Matches recursively a non-quantified RULE-LIST.

Raises a condition when any of the sub-rules are not a match."
  (loop for rule in rule-list
     for match-result = (match-rec rule)
     if (and (not (null match-result))
	     (not (and (listp match-result)
		       (= (list-length match-result) 1)
		       (quantified-rule-p match-result))))
     collect match-result
     else if (null match-result)
     do (syntax-error-condition (line-number *the-head*)
				(column-number *the-head*))))

(defun match-exact (rule)
  "Matches an exact-match RULE. The RULE must be written in
form (:KEYWORD \"token\")."
  (head-checkpoint (*the-head*)
    (let ((result (match-token (cadr rule))))
      (unless (null result) rule))))

(defun match-token (token)
  "Matches a specific TOKEN string."
  (head-match *the-head* token))



;;; Quantified rules

(defun match-try (rule)
  "Recursively matches a RULE. On error, instead of raising
a condition, returns NIL."
  (head-checkpoint (*the-head*)
    (handler-case
	(match-rec rule)
      (syntax-error () nil))))

(defgeneric match-quantifier (quantifier rule-list)
  (:documentation "Matches a certain quantified rule, where
QUANTIFIER is one of :OR, :MAYBE or :MANY, and RULE-LIST is a
list of every other sub-rule that comes after the quantifier."))

(defmethod match-quantifier ((quantifier (eql :or)) rule-list)
  "Recursively matches a quantified :OR rule. The RULE-LIST is
comprised of everything that comes after the quantifier."
  (loop for rule in rule-list
     for match-result = (match-try rule)
     unless (null match-result)
     return match-result))

(defmethod match-quantifier ((quantifier (eql :maybe)) rule-list)
  "Recursively matches a quantified :MAYBE rule. The RULE-LIST is
comprised of everything that comes after the quantifier."
  (let ((result
	 (loop for rule in rule-list
	    for match-result = (match-try rule)
	    if (not (null match-result))
	    collect match-result
	    else return nil)))
    (cons :maybe result)))

(defmethod match-quantifier ((quantifier (eql :many)) rule-list)
  "Recursively matches a quantified :MANY rule. The RULE-LIST is
comprised of everything that comes after the quantifier."
  (let ((result
	 (loop for match-result = (match-try rule-list)
	    until (null match-result)
	    collect match-result)))
    (cons :many result)))

(defun match-quantified (rule)
  "Recursively matches a quantified RULE. The first element of
the RULE must be one of the quantifiers :OR, :MAYBE or :MANY.

Raises a syntax-error condition when an OR-quantified RULE is
not a match."
  (let ((result (match-quantifier (car rule) (cdr rule))))
    (if (and (eql (car rule) :or)
	     (null result))
        (syntax-error-condition (line-number *the-head*)
				(column-number *the-head*))
	result)))



;;; Generic rules

(defun match-compound (rule)
  "Recursively matches a compound RULE. The RULE can be an
exact-match, a quantified rule, or a plain group of rules."
  (cond ((quantified-rule-p rule)
	 (match-quantified rule))
	((exact-match-rule-p rule)
	 (match-exact rule))
	(t (match-group rule))))

(defun match-rec (rule)
  "Recursively matches a grammar RULE of any type."
  (cond ((listp rule)
	 (match-compound rule))
	((stringp rule)
	 (match-token rule))
	((builtin-rule-p rule)
	 (match-builtin rule))
	(t (let ((result (match-rec (grammar-lookup rule))))
	     (unless (null result)
	       (cons rule result))))))


;;; Analyze entry

(defun analyze (file-stream)
  "Takes a FILE-STREAM and syntactically analyzes the file
associated with it, producing a non-optimized syntax tree,
in s-expressions.

This procedure may raise syntax-error and unexpected-eof
conditions."
  (with-new-head (file-stream)
    (match-rec (grammar-lookup :entry))))
