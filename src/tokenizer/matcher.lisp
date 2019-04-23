;;;; matcher.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-tokenizer)

;; (defun match-compare (terminal-rule expected-token)
;;   (let ((token (next-token *tokenizer-head*)))
;;     (if (string= token expected-token)
;; 	(list terminal-rule expected-token)
;; 	(progn (unmatch-token *tokenizer-head* token)
;; 	       nil)))) ; syntax error, unexpected token 'token'

(defun match-compare (terminal-rule expected-token)
  (list :compare terminal-rule expected-token))

;; The following specific matchers should also be recursive. However,
;; it is advised to combine its implementation with proper comparisions,
;; so that we don't end up with a stack overflow

(defun match-or (options)
  (cons :match-or (match options)))

(defun match-many (grammar-rule)
  (cons :match-many grammar-rule))

(defun match-maybe (grammar-rule)
  (cons :match-maybe grammar-rule))

(defun match-quantified (grammar-rule)
  (case (car grammar-rule)
    (:or    (match-or    (cdr grammar-rule)))
    (:many  (match-many  (cdr grammar-rule)))
    (:maybe (match-maybe (cdr grammar-rule)))))

(defun match (grammar-rule)
  (cond ((listp grammar-rule) ; When dealing with a list of rules
	 (if (member (car grammar-rule) *quantifiers*)
	     ;; Dispatch rules with quantifiers
	     (match-quantified grammar-rule)
	     ;; Dispatch subrules, in order
	     (loop for rule in grammar-rule
		;; If second element is string, we match-compare
		if (and (listp rule)
			(stringp (cadr rule)))
		collect (apply #'match-compare rule)
		;; Otherwise match recursively
		else collect (match rule))))
	(t ; For atomic rules, just lookup and match subrules
	 (if (member grammar-rule *builtin-rules*)
	     (list :check grammar-rule)
	     (cons grammar-rule (match (grammar-lookup grammar-rule)))))))
