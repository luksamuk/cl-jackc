;;;; matcher.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-tokenizer)

(defmacro debug-log (warning-msg arguments)
  `(progn
     (format t ,(if (listp arguments)
		    "~a (L~aC~a): ~{~a ~}~%"
		    "~a (L~aC~a): ~a~%")
	     ,warning-msg
	     (line-number *the-head*)
	     (column-number *the-head*)
	     ,arguments)))

;;; Terminal rules

(defun match-builtin (rule)
  (head-checkpoint (*the-head*)
    (let ((match-result (head-match *the-head* rule)))
      (unless (null match-result)
	(list rule match-result)))))

(defun match-group (rule-list)
  (loop for rule in rule-list
     for match-result = (match-rec rule)
     if (and (not (null match-result))
	     (not (and (listp match-result)
		       (= (list-length match-result) 1)
		       (member (car match-result) *quantifiers*))))
     collect match-result
     else if (null match-result)
     do (syntax-error-condition (line-number *the-head*)
				(column-number *the-head*))))

;; TODO: check if (token-class "token") is
;; valid when compiling grammar into hashmap
(defun match-exact (rule)
  (head-checkpoint (*the-head*)
    (let ((result (match-token (cadr rule))))
      (unless (null result) rule))))

(defun match-token (token)
  (head-match *the-head* token))

;;; Quantified rules

(defun match-try (rule)
  (handler-case
      (match-rec rule)
    (syntax-error () nil)))

(defgeneric match-quantifier (quantifier rule-list))

(defmethod match-quantifier ((quantifier (eql :or)) rule-list)
  (loop for rule in rule-list
     for match-result = (match-try rule)
     unless (null match-result)
     return match-result))

(defmethod match-quantifier ((quantifier (eql :maybe)) rule-list)
  (let ((result
	 (loop for rule in rule-list
	    for match-result = (match-try rule)
	    if (not (null match-result))
	    collect match-result
	    else return nil)))
    (cons :maybe result)))

(defmethod match-quantifier ((quantifier (eql :many)) rule-list)
  (let ((result
	 (loop for match-result = (match-try rule-list)
	    until (null match-result)
	    collect match-result)))
    (cons :many result)))

(defun match-quantified (rule)
  (let ((result (match-quantifier (car rule) (cdr rule))))
    (if (and (eql (car rule) :or)
	     (null result))
	(syntax-error-condition (line-number *the-head*)
				(column-number *the-head*))
	result)))


;;; Generic rules

(defun match-compound (rule)
  (cond ((quantified-rule-p rule)
	 (match-quantified rule))
	((exact-match-rule-p rule)
	 (match-exact rule))
	(t (match-group rule))))

(defun match-rec (rule)
  (cond ((listp rule)
	 (match-compound rule))
	((stringp rule)
	 (match-token rule))
	((builtin-rule-p rule)
	 (match-builtin rule))
	(t (let ((result (match-rec (grammar-lookup rule))))
	     (unless (null result)
	       (cons rule result))))))

;;; Entry rule

(defun match-grammar-rules (stream)
  (setf *rule-depth* 0)
  (with-new-head (stream)
    (match-rec *initial-var*)))
