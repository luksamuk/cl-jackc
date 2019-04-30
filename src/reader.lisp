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
	(t (file-not-found-condition path-string))))

(defvar *error-format*
  (concatenate 'string
	       "~tOn file \"~a\":~&"
	       "~t** ~a~&Unexpected condition raised. "
	       "Bailing out.~&")
  "Error format string for general conditions on compilation process.")

(defun read-files (file-list analyze)
    "Dispatches a read stream for each file in FILE-LIST to the
compiler's analyzer. Expects all files to be valid paths."
  (let ((current-stream nil)
	(condition-raised nil))
    (loop for file in file-list
       do (unwind-protect
	       (handler-case
		   (progn (setf current-stream (open file))
			  (format t "~a file ~a...~&"
				  (if analyze "Analyzing" "Compiling")
				  file)
			  ;; (let ((file-proto-ast (analyze current-stream)))
			  ;;   (if analyze
			  ;; 	(parse-as-xml file-proto-ast)
			  ;; 	(parse-as-vm  file-proto-ast)))
			  )
		 (error (err)
		   (setf condition-raised t)
		   (format t *error-format* file err)))
	    (close current-stream))
       when condition-raised do (return nil)
       finally (return t))))
