;;;; grammar.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-tokenizer)



(defparameter *grammar-rules*
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
    (:parameter-list    ((:maybe (:type :var-name)
				 (:many ((:symbol ",")
					 :type :var-name)))))
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
    (:statements        ((:many :statement)))
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
			      :var-name
			      (:var-name (:symbol "[")
					 :expression
					 (:symbol "]"))
			      :subroutine-call
			      ((:symbol "(") :expression (:symbol ")"))
			      (:unary-op :term))))
    (:subroutine-call   ((:or (:subroutine-name
			       (:symbol "(") :expression-list (:symbol ")"))
			      ((:or :class-name :var-name)
			       (:symbol ".") :subroutine-name
			       (:symbol "(") :expression-list (:symbol ")")))))
    (:expression-list   ((:maybe :expression (:many (:symbol ",") :expression))))
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


(defparameter *grammar*
  (loop with hashtable = (make-hash-table)
     for rule in *grammar-rules*
     do (setf (gethash (car rule) hashtable) (cadr rule))
     finally (return hashtable))
  "Holds the de-facto grammar used for lookup. Each grammar rule is the key to an
element on a hash table, and matches exactly one rule list, which may also reference
other grammar rules.")

(defparameter *initial-var* :class
  "Determines the initial rule which the matcher should lookup at the beginning of
every file.")

(defun camelcase-keyword (keyword)
  "Takes a KEYWORD and rewrites it in camel case style."
  (coerce (loop with should-upcase = nil
	     for char across (string-downcase (format nil "~a" keyword))
	     if (char= char #\-)
	     do (setf should-upcase t)
	     else collect (if should-upcase
			      (progn (setf should-upcase nil)
				     (char-upcase char))
			      char))
	  'string))

(defun grammar-lookup (keyword)
  "Looks up KEYWORD in the grammar and returns the rule list associated with it."
  (multiple-value-bind (value unused)
      (gethash keyword *grammar*)
    (declare (ignore unused))
    value))
