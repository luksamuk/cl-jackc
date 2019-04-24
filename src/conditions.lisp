;;;; conditions.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-conditions)

(define-condition file-not-found (error)
  ((%filename :initarg :filename
	      :accessor filename
	      :documentation "Path to the invalid file.")))

(define-condition syntax-error (error)
  ((%linum    :initarg :line
	      :accessor line-number
	      :documentation "Number of the line of the syntax error.")
   (%colnum   :initarg :column
	      :accessor column-number
	      :documentation "Number of the column of the syntax error.")))

(define-condition unexpected-eof (error) ())

(define-condition integer-overflow (syntax-error)
  ((%number   :initarg  :number
	      :accessor blown-number
	      :documentation "Integer value which would overflow.")))


;; Method implementations for readable conditions

(defmethod print-object ((object file-not-found) stream)
  (format stream "Error reading file \"~a\": Not found"
	  (filename object)))

(defmethod print-object ((object syntax-error) stream)
  (format stream "Syntax error on line ~a, column ~a."
	  (line-number object)
	  (column-number object)))

(defmethod print-object ((object unexpected-eof) stream)
  (princ "Unexpected end of file." stream))

(defmethod print-object ((object integer-overflow) stream)
  (format stream "On line ~a, column ~a: Integer overflow (~a is greater than 32767)."
	  (line-number object)
	  (column-number object)
	  (blown-number object)))


