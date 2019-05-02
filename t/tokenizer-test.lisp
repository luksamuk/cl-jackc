;;;; t/tokenizer-test.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:cl-jackc/test)

(defparameter *long-text-eof*
  (format nil "32768~%some text longer than a number"))
  
(deftest tokenizer-test
  (testing "tokens"
    (with-new-head ((open (script-file "tokens")))
      (expected-results
	((match-token "class")       "class")
	((match-token "Bar")         "Bar")
	((match-token "{")           "{")
	((match-token "constructor") "constructor")
	((match-token "Bar")         "Bar")
	((match-token "new")         "new")
	((match-token "(")           "(")
	((match-token ")")           ")")
	((match-token "{")           "{")
	((match-token "return")      "return")
	((match-token "this")        "this")
	((match-token ";")           ";")
	((match-token "}")           "}")
	((match-token "}")           "}"))))
  (testing "integer constant"
    (with-new-head ((open (script-file "numerics")))
      (expected-results
	((match-token "let")             "let")
	((match-token "test")            "test") 
	((match-token "=")               "=")
	((match-token :integer-constant) 12345)
	((match-token ";")               ";"))))
  (testing "integer constant overflow"
    (with-new-head ((open (script-file "intoverflow")))
      (ok (signals (match-token :integer-constant) 'integer-overflow))))
  (testing "string constant"
    (with-new-head ((open (script-file "strings")))
      (expected-results
	((match-token "let")             "let")
	((match-token "something")       "something")
	((match-token "=")               "=")
	((match-token :string-constant)  "\"My String\"")
	((match-token ";")               ";"))))
  (testing "line and column numbers"
    (with-new-head ((open (script-file "tokens")))
      (match-token "class")
      (ok (= (line-number *the-head*) 0))
      (ok (= (column-number *the-head*) 5))
      (mapc (lambda (x) (match-token x))
	    '("Bar" "{" "constructor" "Bar" "new" "(" ")" "{" "return"))
      (ok (= (line-number *the-head*) 2))
      (ok (= (column-number *the-head*) 14))))
  (testing "identifier"
    (with-new-head ((open (script-file "tokens")))
      (expected-results
	((match-token "class")       "class")
	((match-token :identifier)   "Bar")
	((match-token "{")           "{")
	((match-token "constructor") "constructor")
	((match-token :identifier)   "Bar")
	((match-token :identifier)   "new")
	((match-token "(")           "("))))
  (testing "separated-identifier"
    (with-new-head ((open (script-file "identifier")))
      (expected-results
	((match-token :identifier)   "Class")
	((match-token ".")           ".")
	((match-token :identifier)   "thing"))))
  (testing "unexpected eof"
    (with-new-head ((open (script-file "intoverflow")))
      (ok (signals (match-token *long-text-eof*) 'unexpected-eof))))
  (testing "comments"
    (with-new-head ((open (script-file "comments")))
      (expected-results
	((match-token "class")        "class")
	((match-token :identifier)    "Bar")
	((match-token "{")            "{")
	((match-token "static")       "static")
	((match-token "int")          "int")
	((match-token :identifier)    "something")
	((match-token ";")            ";")
	((match-token "constructor")  "constructor")
	((match-token :identifier)    "Bar")
	((match-token :identifier)    "new")
	((match-token "(")            "(")
	((match-token ")")            ")")
	((match-token "{")            "{")
	((match-token "return")       "return")
	((match-token "this")         "this")
	((match-token ";")            ";")
	((match-token "}")            "}")
	((match-token "}")            "}")))))
