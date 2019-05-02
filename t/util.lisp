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


(defun identical-p (a b)
  (and (equal (type-of a) (type-of b))
       (equal a b)))

(defun depth-equal-p (list1 list2)
  (when (= (list-length list1)
	   (list-length list2))
    (loop for elt1 in list1
       for elt2 in list2
       always (cond ((and (listp elt1)
			  (listp elt2))
		     (depth-equal-p elt1 elt2))
		    (t (identical-p elt1 elt2))))))
