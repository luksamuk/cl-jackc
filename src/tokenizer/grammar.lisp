;;;; grammar.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-tokenizer)

(defvar *grammar-rules*
  ;;;  Lexical elements
  '((:keyword          ((:or "class" "constructor" "function"
			     "method" "field" "static" "var"
			     "int" "char" "boolean" "void"
			     "true" "false" "null" "this" "let"
			     "do" "if" "else" "while" "return")))
    (:symbol           ((:or "{" "}" "(" ")" "[" "]" "." ","
			     ";" "+" "-" "*" "/" "&" "|" "<"
			     ">" "=" "~")))
    ;; Builtin lexical elements:
    ;; :integer-constant
    ;; :string-constant
    ;; :identifier
    
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
			      (:keyword "null") (:keyword "this"))))))

;;;; Interpreting grammars:
;;; (:or       . statements)
;;; (:many     . statements)
;;; (:maybe    . statements)
;;; (list-stmt . statements)
;;; (atom-stmt . statements)

;;; list-stmt:
;;; (atom-stmt . statements)
;;; - match atom-stmt:
;;;   => (:or, :many, :maybe): quantifiers
;;;   => keyword such that (grammar-lookup keyword) is not nil
;;;      - match with (grammar-lookup keyword)
;;;   => syntax error


(defparameter *grammar*
  (loop with hashtable = (make-hash-table)
     for rule in *grammar-rules*
     do (setf (gethash (car rule) hashtable) (cadr rule))
     finally (return hashtable)))

(defparameter *initial-var* :class)

(defun camelcase-keyword (keyword)
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
  (multiple-value-bind (value unused)
      (gethash keyword *grammar*)
    (declare (ignore unused))
    value))

;; TODO:
;; integer-constant-p
;; string-constant-p
;; identifier-p
