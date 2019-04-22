;;;; cl-jackc.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:cl-jackc)

(defmacro if-let (binding consequent alternative)
  "Binds a BINDING. If the bound symbol has non-false value, executes
CONSEQUENT; otherwise, executes ALTERNATIVE."
  (destructuring-bind ((sym val)) binding
    `(let ((,sym ,val))
       (if ,sym
	   ,consequent
	   ,alternative))))

(defun find-files (path-string)
  "Finds all Jack files on a given path. If PATH-STRING refers to a
single file, returns a list containing its path object. If it is a
directory, returns a list containing all the Jack files on it."
  (cond ((and (uiop:file-exists-p path-string)
	      (equal (search ".jack" path-string :from-end t)
		     (- (length path-string) 5)))
	 (if-let ((file-path path-string))
	   file-path
	   nil))
	((uiop:directory-exists-p path-string)
	 (directory (concatenate 'string path-string "/*.jack")))
	(t (error "Cannot open file: ~a" path-string))))

(defun compile-exec (file-or-dir &key (analyze t)) ; TODO: default :analyze to nil
  "Compiles a file or directory. Output files will be written in the
same directory as the Jack source files.
Use ANALYZE to specify if the output file is supposed to be just an
XML file for inspection."
  (handler-case
      (if-let ((file-list (find-files file-or-dir)))
	(progn (format t "~a: ~a~&"
		       (if analyze "Analyzing" "Compiling")
		       file-list)
	       ;; TODO: invoke reader for file list
	       )
	(format t "Could not find any Jack files. Compilation aborted.~&"))
    (error (err) ; TODO: improve error handling with custom errors
      (format t "~a~&" err)
      (format t "Error compiling project. Bailing out."))))
