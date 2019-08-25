;;;; parser.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-parser)

;;; Syntax tree cleanup

(defparameter *removed-tags*
  '(:statement :subroutine-call :class-name :many :maybe :subroutine-name
    :type :var-name :op :keyword-constant :unary-op)
  "Collection of tags which should be removed when exporting the AST to
any format.")

(defun cleanup-ast (syntax-tree)
  "Cleans a certain abstract syntax tree coming from the analyzer
module."
  (cond ((null syntax-tree) nil)
	((or (null (car syntax-tree))
	     (member (car syntax-tree) *removed-tags*))
	 (cleanup-ast (cdr syntax-tree)))
	((listp (car syntax-tree))
	 (cons (cleanup-ast (car syntax-tree))
	       (cleanup-ast (cdr syntax-tree))))
	(t (cons (car syntax-tree)
		 (cleanup-ast (cdr syntax-tree))))))


;;; XML emitter (analysis-only case)

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

(defun print-xml-tag (keyword closingp)
  "Prints a XML tag to the standard output.
KEYWORD will be the name of the tag, and will be rewritten in camel case.
CLOSINGP informs whether it is a closing XML tag."
  (when closingp (princ #\Space))
  (princ #\<)
  (when closingp (princ #\/))
  (princ (camelcase-keyword keyword))
  (princ #\>)
  (unless closingp (princ #\Space))
  (terpri))

(defun remove-quotes-strconst (string)
  "Takes the STRING of a string constant match and removes surrounding
quotes."
  (subseq string 1 (1- (length string))))


(defun ast->xml (ast)
    "Takes a clean abstract syntax tree and prints a XML version of it to
the standard output."
  (cond ((null ast) nil)
	((keywordp (car ast))
	 (print-xml-tag (car ast) nil)
	 (ast->xml (cdr ast))
	 (print-xml-tag (car ast) t))
	((listp (car ast))
	 (ast->xml (car ast))
	 (ast->xml (cdr ast)))
	((stringp (car ast))
	 (cond ((string= (car ast) "<")
		(princ " &lt; "))
	       ((string= (car ast) ">")
		(princ " &gt; "))
	       ((string= (car ast) "&")
		(princ " &amp; "))
	       (t (princ (string-trim "\"" (car ast)))))
	 (ast->xml (cdr ast)))
	(t (princ (car ast))
	   (ast->xml (cdr ast)))))

(defun parse-as-xml (syntax-tree)
  "Takes a syntax tree from the analyzer, then cleans it and prints it
to a string, in XML format."
  (setf syntax-tree (cleanup-ast syntax-tree))
  (with-output-to-string (*standard-output*)
    (ast->xml syntax-tree)))


;;; Sexp emitter (analysis-only case)

(defun parse-as-sexp (syntax-tree)
  "Takes a syntax tree from the analyzer, then cleans it and prints it
to a string, in s-expression format."
  (with-output-to-string (*standard-output*)
    (prin1 (cleanup-ast syntax-tree))))


;;; VM emitter (compilation case)

(defun parse-as-vm (syntax-tree)
  (error "Compilation not implemented!"))
