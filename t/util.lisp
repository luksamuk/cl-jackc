;;;; t/util.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:cl-jackc/test)

(defparameter *system-pathname*
  (asdf:system-source-directory :cl-jackc/test))

(defun script-file (file-name)
  (merge-pathnames (concatenate 'string "t/scripts/" file-name)
		   *system-pathname*))

(defmacro expected-results (&body body)
  `(progn
     ,@(loop for case in body
	  collect `(ok (equal ,(car case) ,(cadr case))))))

(defmacro match-token (test)
  `(head-match *the-head* ,test))

(defmacro write-analyzer-test-cases (&rest filenames)
  `(progn
     ,@(loop for filename in filenames
	  collect `(ok (analyze (open (script-file ,filename)))))))

(defparameter *long-text-eof*
  (format nil "32768~%some text longer than a number"))

