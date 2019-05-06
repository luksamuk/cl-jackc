;;;; cl-jackc.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:cl-jackc)

(defun compile-exec (file-or-dir &key (analyze :xml)) ; TODO: default :analyze to nil
  "Compiles a file or directory. Output files will be written in the
same directory as the Jack source files.
Use ANALYZE to specify if the output file is supposed to be just an
XML file for inspection."
  (handler-case
      (progn
	(when (and analyze
		   (not (or (eql analyze :xml) (eql analyze :sexp))))
	  (error (format nil "Unknown analysis case: ~a" analyze)))
	(if-let ((file-list (jackc-reader:find-files file-or-dir)))
	  (jackc-reader:read-files file-list :analyze analyze)
	  (file-not-found-condition file-or-dir)))
    (error (err)
      (format t "~a~&" err)
      (format t "Error compiling project. Bailing out."))))
