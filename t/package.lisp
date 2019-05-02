;;;; t/package.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(defpackage #:cl-jackc/test
  (:use #:cl
	#:jackc-conditions
	#:jackc-tokenizer
	#:jackc-analyzer
	#:jackc-parser
	#:rove))
