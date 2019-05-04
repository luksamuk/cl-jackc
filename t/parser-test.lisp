;;;; t/parser-test.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:cl-jackc/test)

;; (defparameter *parser-expected-valid*
;;   '(:class (:keyword "class") (:class-name (:identifier "Foo"))
;;      (:symbol "{")
;;      ((:class-var-dec (:keyword "static")   (:type (:keyword "int"))
;;        (:var-name     (:identifier "a"))    (:symbol ";"))
;;       (:class-var-dec (:keyword "field")
;;        (:type         (:class-name (:identifier "String")))
;;        (:var-name     (:identifier "name")) (:symbol ";")))
     
;;      ((:subroutine-dec       (:keyword "constructor")
;;        (:type                (:class-name (:identifier "Foo")))
;;        (:subroutine-name     (:identifier "new")) (:symbol "(")
;;        (:parameter-list)     (:symbol ")")
;;        (:subroutine-body     (:symbol "{")
;; 	(:statements
;; 	 (:statement
;; 	  (:let-statement    (:keyword "let")
;; 	   (:var-name        (:identifier "a")) (:symbol "=")
;; 	   (:expression
;; 	    ((:term (:var-name (:identifier "a")))
;; 	     (((:op (:symbol "+")) (:term (:integer-constant 1))))))
;; 	   (:symbol ";")))
;; 	 (:statement
;; 	  (:let-statement    (:keyword "let") (:var-name (:identifier "name"))
;; 	   (:symbol "=")
;; 	   (:expression      (:term (:string-constant "\"Hello!\"")))
;; 	   (:symbol ";")))
;; 	 (:statement
;; 	  (:return-statement (:keyword "return")
;; 	   ((:expression
;; 	     (:term (:keyword-constant (:keyword "this")))))
;; 	    (:symbol ";"))))
;; 	(:symbol "}")))
      
;;       (:subroutine-dec          (:keyword "method") (:keyword "void")
;;        (:subroutine-name        (:identifier "dispose")) (:symbol "(")
;;        (:parameter-list)        (:symbol ")")
;;        (:subroutine-body        (:symbol "{")
;; 	(:statements
;; 	 (:statement
;; 	  (:do-statement        (:keyword "do")
;; 	    (:subroutine-call
;; 	     ((:class-name      (:identifier "Memory")) (:symbol ".")
;; 	      (:subroutine-name (:identifier "deAlloc")) (:symbol "(")
;; 	      (:expression-list
;; 	       (:expression     (:term (:keyword-constant (:keyword "this"))))
;; 	       nil)
;; 	      (:symbol ")")))
;; 	    (:symbol ";")))
;; 	 (:statement (:return-statement (:keyword "return") (:symbol ";"))))
;; 	(:symbol "}")))
      
;;       (:subroutine-dec (:keyword "method") (:keyword "void")
;;        (:subroutine-name (:identifier "showName")) (:symbol "(")
;;        (:parameter-list) (:symbol ")")
;;        (:subroutine-body (:symbol "{")
;; 	(:statements
;; 	 (:statement
;; 	  (:do-statement (:keyword "do")
;; 	    (:subroutine-call
;; 	     ((:class-name (:identifier "Output")) (:symbol ".")
;; 	      (:subroutine-name (:identifier "printString")) (:symbol "(")
;; 	      (:expression-list
;; 	       (:expression
;; 		(:term (:string-constant "\"Hello, my name is \"")))
;; 	       nil)
;; 	      (:symbol ")")))
;; 	     (:symbol ";")))
;; 	  (:statement
;; 	   (:do-statement (:keyword "do")
;; 	     (:subroutine-call
;; 	      ((:class-name (:identifier "Output")) (:symbol ".")
;; 	       (:subroutine-name (:identifier "printString")) (:symbol "(")
;; 	       (:expression-list
;; 		(:expression (:term (:var-name (:identifier "name")))) nil)
;; 	      (:symbol ")")))
;; 	     (:symbol ";")))
;; 	  (:statement
;; 	   (:do-statement (:keyword "do")
;; 	     (:subroutine-call
;; 	      ((:class-name (:identifier "Output")) (:symbol ".")
;; 	       (:subroutine-name (:identifier "println")) (:symbol "(")
;; 	       (:expression-list) (:symbol ")")))
;; 	     (:symbol ";")))
;; 	  (:statement (:return-statement (:keyword "return") (:symbol ";"))))
;; 	(:symbol "}"))))
;;      (:symbol "}")))

(defparameter *parser-expected-valid*
  '(:class (:keyword "class") (:identifier "Foo")
     (:symbol "{")
    ((:class-var-dec (:keyword "static")   (:type (:keyword "int"))
       (:identifier "a") (:symbol ";"))
      (:class-var-dec (:keyword "field") (:type (:identifier "String"))
       (:identifier "name") (:symbol ";")))
     
    ((:subroutine-dec (:keyword "constructor") (:type (:identifier "Foo"))
       (:identifier "new") (:symbol "(") (:parameter-list) (:symbol ")")
       (:subroutine-body (:symbol "{")
	(:statements
	 (:statement
	  (:let-statement (:keyword "let") (:identifier "a") (:symbol "=")
	   (:expression
	    ((:term (:identifier "a"))
	     (((:op (:symbol "+")) (:term (:integer-constant 1))))))
	   (:symbol ";")))
	 (:statement
	  (:let-statement (:keyword "let") (:identifier "name") (:symbol "=")
	   (:expression (:term (:string-constant "\"Hello!\""))) (:symbol ";")))
	 (:statement
	  (:return-statement (:keyword "return")
	   ((:expression (:term (:keyword-constant (:keyword "this")))))
	    (:symbol ";"))))
	(:symbol "}")))
      
      (:subroutine-dec (:keyword "method") (:keyword "void")
       (:identifier "dispose") (:symbol "(") (:parameter-list) (:symbol ")")
       (:subroutine-body (:symbol "{")
	(:statements
	 (:statement
	  (:do-statement (:keyword "do")
	    (:subroutine-call
	     ((:identifier "Memory") (:symbol ".") (:identifier "deAlloc")
	      (:symbol "(")
	      (:expression-list
	       (:expression (:term (:keyword-constant (:keyword "this")))) nil)
	      (:symbol ")")))
	    (:symbol ";")))
	 (:statement (:return-statement (:keyword "return") (:symbol ";"))))
	(:symbol "}")))
      
      (:subroutine-dec (:keyword "method") (:keyword "void")
       (:identifier "showName") (:symbol "(") (:parameter-list) (:symbol ")")
       (:subroutine-body (:symbol "{")
	(:statements
	 (:statement
	  (:do-statement (:keyword "do")
	    (:subroutine-call
	     ((:identifier "Output") (:symbol ".") (:identifier "printString")
	      (:symbol "(")
	      (:expression-list
	       (:expression (:term (:string-constant "\"Hello, my name is \"")))
	       nil)
	      (:symbol ")")))
	     (:symbol ";")))
	  (:statement
	   (:do-statement (:keyword "do")
	     (:subroutine-call
	      ((:identifier "Output") (:symbol ".") (:identifier "printString")
	       (:symbol "(")
	       (:expression-list (:expression (:term (:identifier "name"))) nil)
	      (:symbol ")")))
	     (:symbol ";")))
	  (:statement
	   (:do-statement (:keyword "do")
	     (:subroutine-call
	      ((:identifier "Output") (:symbol ".") (:identifier "println")
	       (:symbol "(") (:expression-list) (:symbol ")")))
	     (:symbol ";")))
	  (:statement (:return-statement (:keyword "return") (:symbol ";"))))
	(:symbol "}"))))
     (:symbol "}")))


(deftest parser-test
  (testing "string-examples"
    (with-input-from-string (class-only-stream "class Bar {}")
      (ok (depth-equal-p (cleanup-ast (analyze class-only-stream))
			 '(:class (:keyword "class") (:identifier "Bar")
			   (:symbol "{") (:symbol "}")))))
    (with-input-from-string (one-static-classvar-stream
			     "class Bar { static int baz; }")
      (ok (depth-equal-p (cleanup-ast (analyze one-static-classvar-stream))
			 '(:class (:keyword "class") (:identifier "Bar")
			   (:symbol "{")
			   ((:class-var-dec (:keyword "static") (:type (:keyword "int"))
			     (:identifier "baz") (:symbol ";")))
			   (:symbol "}")))))
    (with-input-from-string (one-static-method-stream
			     "class Bar { function void foo() { return; } }")
      (ok (depth-equal-p (cleanup-ast (analyze one-static-method-stream))
			 '(:class (:keyword "class") (:identifier "Bar")
			   (:symbol "{")
			   ((:subroutine-dec (:keyword "function") (:keyword "void")
			     (:identifier "foo") (:symbol "(") (:parameter-list)
			     (:symbol ")")
			     (:subroutine-body
			      (:symbol "{")
			      (:statements
			       (:statement (:return-statement (:keyword "return")
							      (:symbol ";"))))
			      (:symbol "}"))))
			   (:symbol "}"))))))
  (testing "valid.jack"
    (let ((ast (cleanup-ast (analyze (open (script-file "valid.jack"))))))
      (ok (depth-equal-p ast *parser-expected-valid*)))))
