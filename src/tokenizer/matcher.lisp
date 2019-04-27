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

;; (defun match-compare (terminal-rule &optional expected-token)
;;   (list :compare terminal-rule expected-token))

;; rule-pair := (rule expected-token)
;; rule      := builtin | or-rule



;; Compare when terminal
(defun match-compare (rule-pair &key (error-p t))
  (format t "Rule-pair: ~a~&" rule-pair)
  (let* ((rule (car rule-pair))
	 (expected-token (cadr rule-pair))
	 (match-result
	 (if (null expected-token)
	     (head-match *the-head* rule)
	     (head-match *the-head* expected-token))))
    (cond ((or (null match-result)
	       (and (not (null expected-token))
		    (not (string= match-result expected-token))))
	   (when error-p
	     (syntax-error-condition (line-number *the-head*)
				     (column-number *the-head*))))
	  (t (format t "*** MATCH: ~a~&" (list rule match-result))
	   (list rule match-result)))))
      
;; (defun match-check (rule-pair &key (error-p t))

;; The following specific matchers should also be recursive. However,
;; it is advised to combine its implementation with proper comparisions,
;; so that we don't end up with a stack overflow

;; Match any of
;; (defun match-or (options)
;;   (cons :match-or (match options)))
(defun match-expand-or (grammar-rules)
  (loop for rule in grammar-rules
     for match-result = (car (match-list (list rule) nil nil))
     if (and (not (null match-result))
	     (if (listp match-result)
		 (not (null (cadr match-result)))
		 t))
     return (progn
	      (format t "   -- or: got ~a~&" match-result)
	      match-result)))

(defun match-or (grammar-rules error-p)
  (format t "  -- enter match-or~&")
  (let ((match-result (match-expand-or grammar-rules)))
    (if (null match-result)
	(when error-p
	  (syntax-error-condition (line-number *the-head*)
				  (column-number *the-head*)))
	match-result)))

;; Appears 0 or 1 times
(defun match-maybe (grammar-rule error-p)
  (format t "  -- enter match-maybe~&")
  (head-checkpoint (*the-head*)
    (loop with match-results = nil
       for rule in grammar-rule
       do (progn
	    (push (match-list (list rule) error-p t) match-results)
	    (format t "   -- maybe: got ~a~&" (car match-results)))
       never (if (listp (car match-results))
		 (null (cadr match-results))
		 (null (car match-results)))
       finally (return (reverse match-results)))))

(defun match-many (grammar-rule fail-on-first)
  (format t "  -- enter match-many~&")
  (loop for current-match = (match-maybe grammar-rule nil)
     until (null current-match)
     collect current-match))

(defun match-quantified (grammar-rule error-p fail-on-first)
  (case (car grammar-rule)
    (:or    (match-or    (cdr grammar-rule) error-p))
    (:maybe (match-maybe (cdr grammar-rule) error-p))
    (:many  (match-many  (cdr grammar-rule) fail-on-first))))

(defun match-list (rules-list error-p fail-on-first)
  (cond ((not (listp rules-list))
	 (format t "Calling match-rec for ~a~&" rules-list)
	 (match-rec rules-list :error-p error-p :fail-first fail-on-first))
	((member (car rules-list) *quantifiers*)
	 (handler-case
	     (match-quantified rules-list error-p fail-on-first)
	   (unexpected-eof () nil)))
	((stringp (cadr rules-list))
	 (match-compare rules-list :error-p error-p))
	(t
	 (loop for rule in rules-list
	    for match-result =
	      (match-list rule error-p t)
	    if (and fail-on-first
		    (or (null match-result)
			(and (listp match-result)
			     (null (cadr match-result)))))
	    do (return nil)
	    else collect
	      (progn
		(format t "  -- match-list got: ~a, continue:~a~&"
			match-result
			(not fail-on-first))
		match-result)))))

(defun match-rec (grammar-rule &key (error-p t) (fail-first nil))
  (format t "-- Match-rec: ~a (L~aC~a), err:~a~&"
	  grammar-rule
	  (line-number *the-head*)
	  (column-number *the-head*)
	  error-p)
  (cond ((null grammar-rule) nil)
	((listp grammar-rule) ; When dealing with a list of rules
	 (match-list grammar-rule error-p fail-first))
	(t ; For atomic rules, just lookup and match subrules
	 (if (member grammar-rule *builtin-rules*)
	     (handler-case 
		 (match-compare (list grammar-rule) :error-p error-p)
	       (unexpected-eof () nil))
	     (progn
	       (format t "-- Atomic non-builtin: ~a, err:~a~&" grammar-rule error-p)
	       (cons grammar-rule
		     (match-rec (grammar-lookup grammar-rule)
				:error-p error-p
				:fail-first fail-first)))))))

(defun match (stream)
  (with-new-head (stream)
    (match-rec *initial-var*)))
