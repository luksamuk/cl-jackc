;;;; reader.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-reader)

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

(defun read-files (file-list)
  "Dispatches a read stream for each file in FILE-LIST to the
compiler's analyzer. Expects all files to be valid paths."
  (loop for file in file-list
     for stream = (open file)
     ;; TODO: Call analyzer for file
     do (format t "Opened stream ~a~&" stream)
     do (close stream)))
