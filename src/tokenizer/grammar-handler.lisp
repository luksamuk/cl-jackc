;;;; grammar-handler.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-tokenizer)

(defvar *quantifiers*   '(:or :many :maybe)
  "Enumerates quantifiers for each rule.")

(defvar *builtin-rules* '(:integer-constant :string-constant :identifier)
  "Enumerates builtin lexical elements which are implemented on the matching
engine.")

(defparameter *initial-var* :class
  "Determines the initial rule which the matcher should lookup at the beginning of
every file.")

(defparameter *grammar* nil
  "Holds the de-facto grammar used for lookup. Each grammar rule is the key to an
element on a hash table, and matches exactly one rule list, which may also reference
other grammar rules.")

(defun grammar-lookup (keyword)
  "Looks up KEYWORD in the grammar and returns the rule list associated with it."
  (unless (null keyword)
    (multiple-value-bind (value unused)
	(gethash keyword *grammar*)
      (declare (ignore unused))
      value)))

(defun grammar-get-comments ()
  "Looks up the :comment rule in the grammar and returns the tokens associated with
it."
  (if-let ((lookup-rule (grammar-lookup :comment)))
    (and (listp lookup-rule)
	 (= (list-length lookup-rule) 1)
	 (listp (car lookup-rule))
	 (eql (caar lookup-rule) :or)
	 (cdar lookup-rule))))

(defun builtin-rule-p (rule)
  "Checks if a certain grammar RULE is a built-in rule."
  (when (member rule *builtin-rules*) t))

(defun quantifier-p (symbol)
  "Checks if a certain SYMBOL is a grammar quantifier."
  (when (member symbol *quantifiers*) t))

(defun quantified-rule-p (rule-list)
  "Checks if a certain RULE-LIST is a quantified list of rules."
  (quantifier-p (car rule-list)))

(defun exact-match-rule-p (rule-list)
  "Checks if a certain RULE-LIST is an exact-match rule (a token-only rule followed
by the expected token)."
  (and (= (list-length rule-list) 2)
       (or (stringp (cadr rule-list))
	   (numberp (cadr rule-list)))))

(defun comment-rule-p (rule-list)
  "Checks if a certain RULE-LIST is a comment token definition."
  (and (= (list-length rule-list) 2)
       (eql (car rule-list) :comment)))

(defun slurp-grammar-file (file-path)
  "Takes a grammar language file FILE-PATH, relative to project directory, and
slurps its s-expressions into a list, which would represent an alist of grammar
rules. This function should preferably be invoked on compile-time."
  (let ((true-path
	 (merge-pathnames file-path
			  (asdf:system-source-directory :cl-jackc))))
    (with-open-file (stream true-path)
      (loop for rule = (handler-case
			   (read stream)
			 (error () nil))
	 while rule
	 collect (if (not (and (listp rule)
			       (= (list-length rule) 2)))
		     (grammar-error-condition
		      "Each grammar rule must be a list of two elements.")
		     rule)))))
			 

(defun compile-grammar (grammar-rules)
  "Compiles the grammar rules of a language, checking exact-match cases for
validity. Generates a hash table with all the rules for faster consulting."
  (labels ((find-rule (rule-name)
	     (if-let ((pre-match (assoc rule-name grammar-rules)))
	       (if (and (listp (cadr  pre-match))
			(listp (caadr pre-match)))
		   (caadr pre-match)
		   (grammar-error-condition
		    (format nil "Grammar syntax error on rule ~a."
			    rule-name)))
	       (grammar-error-condition
		(format nil "The rule ~a does not exist in grammar."
			rule-name))))
	   
	   (test-exact-match (rule)
	     (let ((match (find-rule (car rule))))
	       (cond ((not (eql (car match) :or))
		      (grammar-error-condition
		       (format nil
			       (concatenate 'string
					    "The rule ~a does not represent "
					    "a disjunction of terminals.")
			       (car rule))))
		     ((not (member (cadr rule) (cdr match) :test #'equal))
		      (grammar-error-condition
		       (format nil (concatenate 'string
						"The terminal ~a is not part of "
						"the rule ~a.")
			       (cadr rule)
			       (car rule))))
		     (t t))))

	   (test-comment-rule (rule)
	     (unless (and
		      (listp (cadr rule))
		      (= (list-length (cadr rule)) 1)
		      (listp (caadr rule))
		      (eql (caaadr rule) :or)
		      (let ((comment-pairs (cdaadr rule)))
			(loop for pair in comment-pairs
			   always (and (listp pair)
				       (loop for token in pair
					  always (stringp token))))))
	       (grammar-error-condition
		"The COMMENT tokens rule is ill-defined.")))
	   
	   (traverse-check-rules (rule-list)
	     (loop for rule in rule-list
		when (listp rule)
		do (cond ((exact-match-rule-p rule)
			  (test-exact-match rule))
			 ((comment-rule-p rule)
			  (test-comment-rule rule))
			 (t (traverse-check-rules rule)))))

	   (builtin-rules-redefined-p (rule-list)
	     (not (every #'null
			 (mapcar (lambda (x) (assoc x rule-list))
				 *builtin-rules*))))
	   
	   (obligatory-rules-defined-p (rule-list)
	     (notany #'null
		     (mapcar (lambda (x)
			       (and (not (null x))
				    (listp x)
				    (listp (cadr x))
				    (listp (caadr x))
				    (eql (caaadr x) :or)
				    ;; Best to handle repeated terminals here.
				    (if (not (setp (cdaadr x) :test #'equal))
					(grammar-error-condition
					 (concatenate 'string
						      "Obligatory rules cannot "
						      "have repeated terminal "
						      "strings."))
					t)))
			     (list (assoc :keyword rule-list)
				   (assoc :symbol rule-list))))))

    ;; Ensure obligatory rules. Plus ensure that their terminal
    ;; strings do not repeat.
    (unless (obligatory-rules-defined-p grammar-rules)
      (grammar-error-condition
       (format
	nil
	"One or more obligatory rules missing.~%Please define :KEYWORD and :SYMBOL.")))

    ;; Ensure no redefinition of built-in rules
    (when (builtin-rules-redefined-p grammar-rules)
      (grammar-error-condition
       (format
	nil
	"One or more built-in rules were redefined.~%Ensure there are no rules such as ~a."
	*builtin-rules*)))
    
    ;; Check all exact-matches recursively
    (traverse-check-rules grammar-rules)

    ;; Finally generate a hashtable of rules
    (loop with hashtable = (make-hash-table)
       for rule in grammar-rules
       do (setf (gethash (car rule) hashtable)
		(cadr rule))
       finally (return hashtable))))


;;; Compile grammar right after file is loaded.
;;; This should speed things up a bit when running the compiler as a
;;; Lisp image.
(setf *grammar* (compile-grammar (slurp-grammar-file "grammars/jack.grammar")))
