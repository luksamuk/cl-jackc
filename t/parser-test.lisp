;;;; t/parser-test.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:cl-jackc/test)

;; Unused....
;; (defparameter *parser-expected-valid* ...)


;; (deftest parser-test
;;   (testing "string-examples"
;;     (with-input-from-string (class-only-stream "class Bar {}")
;;       (ok (depth-equal-p (cleanup-ast (analyze class-only-stream))
;; 			 '(:class (:keyword "class") (:identifier "Bar")
;; 			   (:symbol "{") (:symbol "}")))))
;;     (with-input-from-string (one-static-classvar-stream
;; 			     "class Bar { static int baz; }")
;;       (ok (depth-equal-p (cleanup-ast (analyze one-static-classvar-stream))
;; 			 '(:class (:keyword "class") (:identifier "Bar")
;; 			   (:symbol "{")
;; 			   (:class-var-dec (:keyword "static") (:keyword "int")
;; 			    (:identifier "baz") (:symbol ";"))
;; 			   (:symbol "}")))))
;;     (with-input-from-string (one-static-method-stream
;; 			     "class Bar { function void foo() { return; } }")
;;       (ok (depth-equal-p (cleanup-ast (analyze one-static-method-stream))
;; 			 '(:class (:keyword "class") (:identifier "Bar")
;; 			   (:symbol "{")
;; 			   (:subroutine-dec (:keyword "function") (:keyword "void")
;; 			    (:identifier "foo") (:symbol "(") (:parameter-list)
;; 			    (:symbol ")")
;; 			    (:subroutine-body (:symbol "{")
;; 			     (:statements
;; 			      ((:return-statement (:keyword "return")
;; 							     (:symbol ";"))))
;; 			     (:symbol "}")))
;; 			 (:symbol "}"))))))
;;   (testing "valid.jack"
;;     (let ((ast (cleanup-ast (analyze (open (script-file "valid.jack"))))))
;;       (ok (depth-equal-p ast *parser-expected-valid*)))))


;;;; Pertinent to file compilation testing. 

(defun load-xml-file (filename)
  (parse-file filename (make-xmls-builder)))

(defun clean-parsed (tree)
  (cond ((stringp (car tree))
	 (let ((trimmed (string-trim '(#\Space #\Newline #\Tab) (car tree))))
	   (if (string= trimmed "")
	       (clean-parsed (cdr tree))
	       (cons trimmed
		     (clean-parsed (cdr tree))))))
	((and (null (car tree))
	      (null (cdr tree)))
	 nil)
	((listp (car tree))
	 (cons (clean-parsed (car tree))
	       (clean-parsed (cdr tree))))
	(t (cons (car tree)
		 (clean-parsed (cdr tree))))))

(defmacro parser-test-string (string match)
  `(with-input-from-string (stream ,string)
     (let ((clean-ast (cleanup-ast (analyze stream))))
       (ok (equal clean-ast ,match)))))

(deftest parser-string-test
  ;; Plain string tests
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

