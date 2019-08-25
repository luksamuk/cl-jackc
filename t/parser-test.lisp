;;;; t/parser-test.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:cl-jackc/test)

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
(deftest parser-file-test
  (testing "ArrayTest"
    (ok (write-xml-test-case "ArrayTest/Main")))
  (testing "ExpressionLessSquare"
    (ok (write-xml-test-case "ExpressionLessSquare/Main"))
    (ok (write-xml-test-case "ExpressionLessSquare/SquareGame"))
    (ok (write-xml-test-case "ExpressionLessSquare/Square")))
  (testing "Square"
    (ok (write-xml-test-case "Square/Main"))
    (ok (write-xml-test-case "Square/SquareGame"))
    (ok (write-xml-test-case "Square/Square"))))
