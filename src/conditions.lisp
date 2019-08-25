;;;; conditions.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-conditions)

(define-condition file-not-found (error)
  ((%filename :initarg :filename
	      :accessor filename
	      :documentation "Path to the invalid file."))
  (:documentation "Condition for whenever a file compiled by cl-jackc
cannot be found on the filesystem.")
  (:report (lambda (condition stream)
	     (format stream "Error reading file \"~a\": Not found"
		     (filename condition)))))

(define-condition syntax-error (error)
  ((%linum    :initarg :line
	      :accessor line-number
	      :documentation "Number of the line of the syntax error.")
   (%colnum   :initarg :column
	      :accessor column-number
	      :documentation "Number of the column of the syntax error."))
  (:documentation "Condition for whenever a file compilation fails with
a syntax error on a given line and column.")
  (:report (lambda (condition stream)
	     (format stream "Syntax error on line ~a, column ~a."
		     (line-number condition)
		     (column-number condition)))))

(define-condition unexpected-eof (error) ()
  (:documentation "Condition for whenever a file compilation fails due to
a prematurely ending file.")
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (princ "Unexpected end of file." stream))))

(define-condition integer-overflow (syntax-error)
  ((%number   :initarg  :number
	      :accessor blown-number
	      :documentation "Integer value which would overflow."))
  (:documentation "Condition for whenever a file compilation fails due to
an out-of-bounds integer value.")
  (:report (lambda (condition stream)
	     (format stream
		     "On line ~a, column ~a: Integer overflow (~a is greater than 32767)."
		     (line-number condition)
		     (column-number condition)
		     (blown-number condition)))))
	     
(define-condition grammar-error (error)
  ((%description :initarg :description
		 :accessor description
		 :documentation "Description of the grammar error condition."))
  (:documentation "Condition for whenever the language grammar parsing
process fails.")
  (:report (lambda (condition stream)
	     (format stream "Grammar compilation error: ~a"
		     (description condition)))))



;; Helper functions for throwing conditions

(defun file-not-found-condition (filename)
  "Raises an error of type FILE-NOT-FOUND for a FILENAME."
  (error 'file-not-found :filename filename))

(defun syntax-error-condition (line-number column-number)
  "Raises an error of SYNTAX-ERROR for a LINE-NUMBER and a
COLUMN-NUMBER."
  (error 'syntax-error :line line-number :column column-number))

(defun unexpected-eof-condition ()
  "Raises an error of UNEXPECTED-EOF."
  (error 'unexpected-eof))

(defun integer-overflow-condition (line-number column-number blown-number)
  "Raises an error of INTEGER-OVERFLOW for a BLOWN-NUMBER, also pointing
the LINE-NUMBER and COLUMN-NUMBER where it happens."
  (error 'integer-overflow
	 :line line-number
	 :column column-number
	 :number blown-number))

(defun grammar-error-condition (description)
  "Raises an error of GRAMMAR-ERROR."
  (error 'grammar-error
	 :description description))
