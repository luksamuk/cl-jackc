;;;; t/util.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:cl-jackc/test)

(defparameter *system-pathname*
  (asdf:system-source-directory :cl-jackc-test)
  "Path for this testing system.")

(defun script-file (file-name)
  "Generates the path for a script FILE-NAME based on cl-jackc-test
system's path."
  (merge-pathnames (concatenate 'string "t/scripts/" file-name)
		   *system-pathname*))

(defmacro expected-results (&body body)
  "Generates a number of test cases based on list-syntax pairs
on the given BODY. Each case must be a list of two elements which
shall be compared under the EQUAL predicate and checked under rove's
OK macro."
  `(progn
     ,@(loop for case in body
	  collect `(ok (equal ,(car case) ,(cadr case))))))

(defmacro match-token (test)
  "Small macro for attempting to match a token TEST with the tokenizer
head. See src/tokenizer/head.lisp for info on the types of tokens."
  `(head-match *the-head* ,test))

(defmacro write-analyzer-test-cases (&rest filenames)
  "Analyzes a bunch of files based on the FILENAMES given as argument."
  `(progn
     ,@(loop for filename in filenames
	  collect `(ok (analyze (open (script-file ,filename)))))))


(defun identical-p (a b)
  "Tests whether two objects are both of same type and are structurally
similar. Could be redundant."
  (and (equal (type-of a) (type-of b))
       (equal a b)))


(defmacro parser-test-string (string match)
  "Generates an AST from a STRING and tests for structural similarity
with MATCH. The result is then passed to rove's OK macro."
  `(with-input-from-string (stream ,string)
     (let ((clean-ast (cleanup-ast (analyze stream))))
       (ok (equal clean-ast ,match)))))


(defun clean-parsed-xml (tree)
  "Takes a parsed XML tree from cxml and generates another one such that
the strings are left and right trimmed for spaces. This is useful for
structural similarity checking."
  (cond ((stringp (car tree))
	 (let ((trimmed (string-trim '(#\Space #\Newline #\Tab) (car tree))))
	   (if (string= trimmed "")
	       (clean-parsed-xml (cdr tree))
	       (cons trimmed
		     (clean-parsed-xml (cdr tree))))))
	((and (null (car tree))
	      (null (cdr tree)))
	 nil)
	((listp (car tree))
	 (cons (clean-parsed-xml (car tree))
	       (clean-parsed-xml (cdr tree))))
	(t (cons (car tree)
		 (clean-parsed-xml (cdr tree))))))

(defun load-xml-file (filename)
  "Loads an XML file from disk at FILENAME, using cxml, then parses and
cleans it, generating an AST with trimmed strings. Can be used for
structural similarity checks."
  (clean-parsed-xml
   (parse-file filename (make-xmls-builder))))

(defun load-xml-string (string)
  "Takes an XML STRING, parses it using cxml, then cleans it,
generating an AST with trimmed strings. Can be used for structural
similarity checks."
  (clean-parsed-xml
   (parse string (make-xmls-builder))))

(defun book-file (name &optional (xmlp nil))
  "Loads one of the book files of the nand2tetris materials which were
bundled with the compiler. If XMLP is not specified, looks for file
at t/scripts/book/<NAME>.jack. If it is, looks for file at
t/xml/book/<NAME>.xml. This can be used for loading both a script file
or its AST in XML format."
  (merge-pathnames (concatenate 'string
				"t/"
				(if xmlp "xml/" "scripts/")
				"book/"
				name
				(if xmlp ".xml" ".jack"))
		   *system-pathname*))

(defmacro write-xml-test-case (filename)
  "Generates a test case for nand2tetris materials' bundled scripts.
Generates two ASTs, such that the first is from <FILENAME>.jack file
(compiled) and the second is the AST in <FILENAME>.xml, in internal
s-expression representation. The ASTs are then structurally compared.
This does not invoke any forms from rove package."
  `(equal (load-xml-string
	   (parse-as-xml
	    (cleanup-ast (analyze (open (book-file ,filename))))))
	  (load-xml-file (book-file ,filename t))))

(defun export-xmls-as-sexps-for-diff (casefile &optional (out-path "~/"))
  "Test function which exports a test file and its XML expected output as
s-expressions. The .sexp files can be diff'd on the console. orig.sexp
is always the expected output, and comp.sexp is the compiled output."
  (with-open-file (file (merge-pathnames "orig.sexp" out-path)
			:direction :output
			:if-exists :supersede)
    (write (load-xml-file (book-file casefile t)) :stream file))
  (with-open-file (file (merge-pathnames "comp.sexp" out-path)
			:direction :output
			:if-exists :supersede)
    (write (load-xml-string
	    (parse-as-xml
	     (cleanup-ast (analyze (open (book-file casefile))))))
	   :stream file)))
