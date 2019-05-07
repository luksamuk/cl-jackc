;;;; grammar.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-tokenizer)



(defparameter *jack-grammar-rules*
  '(;;;  Lexical elements
    (:keyword          ((:or "class" "constructor" "function"
			     "method" "field" "static" "var"
			     "int" "char" "boolean" "void"
			     "true" "false" "null" "this" "let"
			     "do" "if" "else" "while" "return")))
    (:symbol           ((:or "{" "}" "(" ")" "[" "]" "." ","
			     ";" "+" "-" "*" "/" "&" "|" "<"
			     ">" "=" "~")))
    
    ;;; Program structure
    (:class             ((:keyword "class")
			 :class-name
			 (:symbol "{")
			 (:many :class-var-dec)
			 (:many :subroutine-dec)
			 (:symbol "}")))
    (:class-var-dec     ((:or (:keyword "static")
			      (:keyword "field"))
			 :type
			 :var-name
			 (:many ((:symbol ",") :var-name))
			(:symbol ";")))
    (:type              ((:or (:keyword "int")
			      (:keyword "char")
			      (:keyword "boolean")
			      :class-name)))
    (:subroutine-dec    ((:or (:keyword "constructor")
			      (:keyword "function")
			      (:keyword "method"))
			 (:or (:keyword "void")
			      :type)
			 :subroutine-name
			 (:symbol "(")
			 :parameter-list
			 (:symbol ")")
			 :subroutine-body))
    (:parameter-list    (:maybe (:type :var-name)
				 (:many ((:symbol ",")
					 :type :var-name))))
    (:subroutine-body   ((:symbol "{")
			 (:many :var-dec)
			 :statements
			 (:symbol "}")))
    (:var-dec           ((:keyword "var") :type :var-name
			 (:many (:symbol ",") :var-name)
			 (:symbol ";")))
    (:class-name        (:identifier))
    (:subroutine-name   (:identifier))
    (:var-name          (:identifier))
    
    ;;; Statements
    (:statements        (:many :statement))
    (:statement         ((:or :let-statement :if-statement :while-statement
			      :do-statement :return-statement)))
    (:let-statement     ((:keyword "let") :var-name
			 (:maybe ((:symbol "[")
				  :expression
				  (:symbol "]")))
			 (:symbol "=") :expression (:symbol ";")))
    (:if-statement      ((:keyword "if")
			 (:symbol "(") :expression (:symbol ")")
			 (:symbol "{") :statements (:symbol "}")
			 (:maybe (:keyword "else")
				 (:symbol "{")
				 :statements
				 (:symbol "}"))))
    (:while-statement   ((:keyword "while")
			 (:symbol "(") :expression (:symbol ")")
			 (:symbol "{") :statements (:symbol "}")))
    (:do-statement      ((:keyword "do")
			 :subroutine-call
			 (:symbol ";")))
    (:return-statement  ((:keyword "return")
			 (:maybe :expression)
			 (:symbol ";")))
    
    ;;; Expressions
    (:expression        ((:term (:many (:op :term)))))
    (:term              ((:or :integer-constant
			      :string-constant
			      :keyword-constant
			      (:var-name (:symbol "[")
					 :expression
					 (:symbol "]"))
			      :subroutine-call
			      :var-name
			      ((:symbol "(") :expression (:symbol ")"))
			      (:unary-op :term))))
    (:subroutine-call   ((:or (:subroutine-name
    			       (:symbol "(") :expression-list (:symbol ")"))
    			      ((:or :class-name :var-name)
    			       (:symbol ".") :subroutine-name
    			       (:symbol "(") :expression-list (:symbol ")")))))
    (:expression-list   (:maybe :expression (:many (:symbol ",") :expression)))
    (:op                ((:or (:symbol "+") (:symbol "-") (:symbol "*")
			      (:symbol "/") (:symbol "&") (:symbol "|")
			      (:symbol "<") (:symbol ">") (:symbol "="))))
    (:unary-op          ((:or (:symbol "-") (:symbol "~"))))
    (:keyword-constant  ((:or (:keyword "true") (:keyword "false")
			      (:keyword "null") (:keyword "this"))))
    )
  "Enumerates the grammar rules of the Jack language.")


(defparameter *comment-tokens* '(("//") ("/*" . "*/"))
  "Enumerates a list of comment token pairs, where the first is the opening token,
and the second is the closing token. If no closing token has been specified, then
it is assumed to be the newline character.")

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
       (stringp (cadr rule-list))))

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
	   
	   (traverse-check-rules (rule-list)
	     (loop for rule in rule-list
		when (listp rule)
		do (if (exact-match-rule-p rule)
		       (test-exact-match rule)
		       (traverse-check-rules rule)))))
    
    (traverse-check-rules grammar-rules)
    (loop with hashtable = (make-hash-table)
       for rule in grammar-rules
       do (setf (gethash (car rule) hashtable)
		(cadr rule))
       finally (return hashtable))))


;;; Compile grammar right after file is loaded.
;;; This should speed things up a bit when running the compiler as a
;;; Lisp image.
(setf *grammar* (compile-grammar *jack-grammar-rules*))
