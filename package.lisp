;;;; package.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(defpackage #:jackc-utils
  (:use #:cl)
  (:export #:if-let
	   #:whitespace-p
	   #:numeric-char-p)
  (:documentation
   "Utilities and miscellaneous structures for all other compiler modules,
usually related to language extension."))

(defpackage #:jackc-conditions
  (:use #:cl)
  (:export #:file-not-found
	   #:syntax-error
	   #:unexpected-eof
	   #:integer-overflow
	   #:grammar-error
	   #:print-object
	   #:file-not-found-condition
	   #:syntax-error-condition
	   #:unexpected-eof-condition
	   #:integer-overflow-condition
	   #:grammar-error-condition)
  (:documentation
   "Definitions for compiler-related conditions."))

(defpackage #:cl-jackc
  (:use #:cl
	#:jackc-utils
	#:jackc-conditions)
  (:export #:compile-exec)
  (:documentation
   "Default interface for compiler. Exports procedures so the user can
interact with the compiler."))

(defpackage #:jackc-tokenizer
  (:use #:cl
	#:jackc-utils
	#:split-sequence
	#:jackc-conditions)
  (:export #:tokenizer-head
	   #:column-number
	   #:line-number
	   #:head-match
	   #:head-position
	   #:head-checkpoint
	   #:*the-head*
	   #:with-new-head
	   #:grammar-lookup
	   #:*initial-var*
	   #:builtin-rule-p
	   #:quantifier-p
	   #:quantified-rule-p
	   #:exact-match-rule-p)
  (:documentation
   "Capable of reading specific tokens and handling a file stream, while
also holding the language grammar specification."))

(defpackage #:jackc-analyzer
  (:use #:cl
	#:jackc-conditions
	#:jackc-tokenizer)
  (:export #:analyze)
  (:documentation
   "Takes a single file stream, and matches it with the language
grammar, generating a syntax tree. Redirects the output to the parser."))

(defpackage #:jackc-parser
  (:use #:cl
	#:jackc-tokenizer)
  (:export #:cleanup-ast
	   #:parse-as-xml
	   #:parse-as-sexp
	   #:parse-as-vm)
  (:documentation
   "Takes the output of a file tokenization, and effectively compiles it
to one of the desired outputs (XML, SEXP or VM), outputting it to
console."))

(defpackage #:jackc-writer
  (:use #:cl)
  (:export #:write-file)
  (:documentation
   "Takes the console output of the parser, and writes it to the desired
(.xml, .sexp or .vm) file."))

(defpackage #:jackc-reader
  (:use #:cl
	#:jackc-utils
	#:jackc-conditions
	#:jackc-analyzer
	#:jackc-parser
	#:jackc-writer)
  (:export #:find-files
	   #:read-files)
  (:documentation
   "Takes compilation arguments and configuration, passed by the
user. For each given file, generates a file stream and passes it to
the analyzer."))
