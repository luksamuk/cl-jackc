;;;; writer.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-writer)

(defun transform-filename (original-path analysis-type)
  "Takes the ORIGINAL-PATH of a .jack file and, according to
its ANALYSIS-TYPE, generates an output name for it. When
ANALYSIS-TYPE is NIL, it is assumed that a compilation procedure
was made, and so VM files will be generated.

ORIGINAL-PATH is always assumed to have a .jack extension."
  (setf original-path (namestring original-path))
  (concatenate 'string
	       (subseq original-path
		       0
		       (- (length original-path) 5))
	       (case analysis-type
		 (:xml ".xml")
		 (:sexp ".sexp")
		 (otherwise ".vm"))))
    
(defun write-file (original-path string-output analysis-type)
  "Writes STRING-OUTPUT to a file. Filename is automatically
deduced from ORIGINAL-PATH and ANALYSIS-TYPE.

ORIGINAL-PATH is expected to be a valid path to a .jack file.
ANALYSIS-TYPE is expected to be either :XML or :SEXP. Otherwise,
it is assumed that a VM Translator file is to be generated."
  (with-open-file (file (transform-filename original-path analysis-type)
			:direction         :output
			:if-exists         :supersede
			:if-does-not-exist :create)
    (princ string-output file)))
