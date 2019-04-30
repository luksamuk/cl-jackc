;;;; parser.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-parser)

;;; Syntax tree cleanup

(defun cleanup-ast (syntax-tree)
  (cond ((null syntax-tree) nil)
	((listp (car syntax-tree))
	 (cons (if (and (listp (caar syntax-tree))
			(= (list-length (car syntax-tree)) 1))
		   (cleanup-ast (caar syntax-tree))
		   (cleanup-ast (car syntax-tree)))
	       (cleanup-ast (cdr syntax-tree))))
	((quantifier-p (car syntax-tree))
	 (cleanup-ast (cdr syntax-tree)))
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
  (when closingp (princ #\Space))
  (princ #\<)
  (when closingp (princ #\/))
  (princ (camelcase-keyword keyword))
  (princ #\>)
  (unless closingp (princ #\Space)))

(defun remove-quotes-strconst (string)
  (subseq string 1 (1- (length string))))

(defun ast->xml (clean-syntax-tree)
  (cond ((null clean-syntax-tree))
	((keywordp (car clean-syntax-tree))
	 (print-xml-tag (car clean-syntax-tree) nil)
	 (ast->xml (cdr clean-syntax-tree))
	 (print-xml-tag (car clean-syntax-tree) t))
	((listp (car clean-syntax-tree))
	 (cond ((and (= (list-length (car clean-syntax-tree)) 2)
		     (listp (cadar clean-syntax-tree))
		     (eql (caadar clean-syntax-tree) :identifier))
		(ast->xml (cadar clean-syntax-tree)))
	       ((and (listp (cadar clean-syntax-tree))
		     (eql (caadar clean-syntax-tree) :string-constant))
		(let* ((pair (cadar clean-syntax-tree))
		       (string (cadr pair)))
		  (ast->xml
		   (list :string-constant
			 (remove-quotes-strconst string)))))
	       (t (ast->xml (car clean-syntax-tree))))
	 (ast->xml (cdr clean-syntax-tree)))
	(t (princ (car clean-syntax-tree))
	   (ast->xml (cdr clean-syntax-tree)))))

(defun parse-as-xml (syntax-tree)
  (setf syntax-tree (cleanup-ast syntax-tree))
  (with-output-to-string (*standard-output*)
    (ast->xml syntax-tree)))



;;; VM code emitter

(defun parse-as-vm (syntax-tree)
  (error "Semantic step not implemented"))
