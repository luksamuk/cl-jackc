;;;; cl-jackc.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:cl-jackc)

(defun compile-exec (file-or-dir &key (analyze t)) ; TODO: default :analyze to nil
  "Compiles a file or directory. Output files will be written in the
same directory as the Jack source files.
Use ANALYZE to specify if the output file is supposed to be just an
XML file for inspection."
  (handler-case
      (if-let ((file-list (jackc-reader:find-files file-or-dir)))
	(progn (format t "~a: ~a~&"
		       (if analyze "Analyzing" "Compiling")
		       file-list)
	       (jackc-reader:read-files file-list))
	(format t "Could not find any Jack files. Compilation aborted.~&"))
    (error (err) ; TODO: improve error handling with custom errors
      (format t "~a~&" err)
      (format t "Error compiling project. Bailing out."))))
