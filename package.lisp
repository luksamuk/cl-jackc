;;;; package.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(defpackage #:cl-jackc
  (:use #:cl)
  (:export #:if-let
	   #:compile-exec)
  (:documentation
   "Default interface for compiler. Exports procedures so the user can
interact with the compiler."))

(defpackage #:jackc-reader
  (:use #:cl #:cl-jackc)
  (:export #:find-files
	   #:read-files)
  (:documentation
   "Takes compilation arguments and configuration, passed by the
user. For each given file, generates a file stream and passes it to
the analyzer."))

(defpackage #:jackc-analyzer
  (:use #:cl)
  (:documentation
   "Takes a single file stream, and invokes the tokenizer for
it. Redirects the tokenizer's output to the parser."))

(defpackage #:jackc-tokenizer
  (:use #:cl #:cl-jackc #:split-sequence)
  (:documentation
   "Takes a file stream, produces an alist for it. The alist output
resembles the expected XML after a single analysis step."))

(defpackage #:jackc-parser
  (:use #:cl)
  (:documentation
   "Takes the output of a file tokenization, and effectively compiles it
to one of the desired outputs (XML or VM), outputting it to console."))

(defpackage #:jackc-writer
  (:use #:cl)
  (:documentation
   "Takes the console output of the parser, and writes it to the desired
(.xml or .vm) file."))
