;;;; parser.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-parser)

;;; Syntax tree cleanup

(defun cleanup-ast-1 (syntax-tree)
  "Performs the first pass on cleaning an abstract syntax tree.
Removes all quantifiers and some nested lists."
  (cond ((null syntax-tree) nil)
	((listp (car syntax-tree))
	 (cons (if (and (listp (caar syntax-tree))
			(= (list-length (car syntax-tree)) 1))
		   (cleanup-ast-1 (caar syntax-tree))
		   (cleanup-ast-1 (car syntax-tree)))
	       (cleanup-ast-1 (cdr syntax-tree))))
	((quantifier-p (car syntax-tree))
	 (cleanup-ast-1 (cdr syntax-tree)))
	(t (cons (car syntax-tree)
		 (cleanup-ast-1 (cdr syntax-tree))))))

(defun cleanup-ast-sublist-2 (sublist)
  "Helper for second pass of abstract tree cleanup.
If an identifier match has been enclosed in a rule,
removes such rule and leaves only the identifier."
  (cond ((and (= (list-length sublist) 2)
	      (listp (cadr sublist))
	      (eql (caadr sublist) :identifier))
	 (cleanup-ast-2 (cadr sublist)))
	(t (cleanup-ast-2 sublist))))

(defun cleanup-ast-2 (syntax-tree)
  "Performs the second pass on cleaning an abstract syntax tree.
Decouples identifier-only rules."
  (cond ((null syntax-tree) nil)
	((listp (car syntax-tree))
	 (cons (cleanup-ast-sublist-2 (car syntax-tree))
	       (cleanup-ast-2 (cdr syntax-tree))))
	(t (cons (car syntax-tree)
		 (cleanup-ast-2 (cdr syntax-tree))))))

(defun cleanup-ast (syntax-tree)
  "Cleans a certain abstract syntax tree coming from the analyzer
module."
  (cleanup-ast-2 (cleanup-ast-1 syntax-tree)))



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
  (unless closingp (princ #\Space)))

(defun remove-quotes-strconst (string)
  "Takes the STRING of a string constant match and removes surrounding
quotes."
  (subseq string 1 (1- (length string))))

(defun ast->xml (clean-ast)
  "Takes a clean abstract syntax tree and prints a XML version of it to
the standard output."
  (cond ((null clean-ast) nil)
	((keywordp (car clean-ast))
	 (print-xml-tag (car clean-ast) nil)
	 (ast->xml (cdr clean-ast))
	 (print-xml-tag (car clean-ast) t))
	((listp (car clean-ast))
	 (if (and (exact-match-rule-p (car clean-ast))
		  (eql (caar clean-ast) :string-constant))
	     (ast->xml (list :string-constant
			     (remove-quotes-strconst (cadar clean-ast))))
	     (ast->xml (car clean-ast)))
	 (ast->xml (cdr clean-ast)))
	(t (princ (car clean-ast))
	   (ast->xml (cdr clean-ast)))))
		  


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
