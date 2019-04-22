;;;; tokenizer-state.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-tokenizer)

(defclass tokenizer-state ()
  ((%stream :initarg  :file-stream
	    :initform nil
	    :reader   fstream)
   (%tokbuf :initform nil
	    :accessor token-buffer)
   (%linum  :initform 0
	    :accessor line-number)))

(defgeneric tokenize-line (tokenizer))
(defgeneric next-token    (tokenizer))
(defgeneric unmatch-token (tokenizer token))

(defvar *tokenizer-head* nil)

(defmacro with-new-head ((file-stream) &body body)
  `(let ((*tokenizer-head*
	  (make-instance 'tokenizer-state
			 :file-stream ,file-stream)))
     ,@body))
			 
