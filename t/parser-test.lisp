;;;; t/parser-test.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:cl-jackc/test)

;;;; Pertinent to file compilation testing. 

(defmacro parser-test-string (string match)
  `(with-input-from-string (stream ,string)
     (let ((clean-ast (cleanup-ast (analyze stream))))
       (ok (equal clean-ast ,match)))))

;;; Plain String tests
(deftest parser-string-test
  (testing "class-only"
    (parser-test-string "class Bar {}"
      '(:class (:keyword "class") ((:identifier "Bar"))
	(:symbol "{") (:symbol "}"))))
  (testing "static-class-var"
    (parser-test-string "class Bar { static int baz; }"
      '(:class (:keyword "class") ((:identifier "Bar"))
	(:symbol "{")
	(((:class-var-dec (:keyword "static")
			  ((:keyword "int"))
			  ((:identifier "baz"))
			  (:symbol ";"))))
	(:symbol "}"))))
  (testing "static-method"
    (parser-test-string "class Bar { function void foo() { return; } }"
      '(:class (:keyword "class") ((:identifier "Bar"))
	(:symbol "{")
	(((:subroutine-dec (:keyword "function") (:keyword "void")
			   ((:identifier "foo")) (:symbol "(")
			   (:parameter-list)
			   (:symbol ")")
			   (:subroutine-body (:symbol "{")
					     (:statements (((:return-statement
							     (:keyword "return")
							     (:symbol ";")))))
					     (:symbol "}")))))
	(:symbol "}")))))


;;; File parsing tests
;; Scripts are located at t/<type>/book/, where <type> can be
;; scripts/ (for Jack files) or xml/ (for XML files).

(defun clean-parsed-xml (tree)
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
  (clean-parsed-xml
   (parse-file filename (make-xmls-builder))))

(defun load-xml-string (string)
  (clean-parsed-xml
   (parse string (make-xmls-builder))))

(defun book-file (name &optional (xmlp nil))
  (merge-pathnames (concatenate 'string
				"t/"
				(if xmlp "xml/" "scripts/")
				"book/"
				name
				(if xmlp ".xml" ".jack"))
		   *system-pathname*))

(defmacro build-xml-test-case (filename)
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

(deftest parser-file-test
  (testing "ArrayTest"
    (ok (build-xml-test-case "ArrayTest/Main")))
  (testing "ExpressionLessSquare"
    (ok (build-xml-test-case "ExpressionLessSquare/Main"))
    (ok (build-xml-test-case "ExpressionLessSquare/SquareGame"))
    (ok (build-xml-test-case "ExpressionLessSquare/Square")))
  (testing "Square"
    (ok (build-xml-test-case "Square/Main"))
    (ok (build-xml-test-case "Square/SquareGame"))
    (ok (build-xml-test-case "Square/Square"))))
