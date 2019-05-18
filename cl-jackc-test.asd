;;;; cl-jackc-test.asd
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(asdf:defsystem #:cl-jackc-test
  :description "Test system for cl-jackc Jack compiler"
  :author "Lucas Vieira <lucasvieira@protonmail.com>"
  :license "MIT"
  :version "0.4.5"
  :serial t
  :depends-on (#:cl-jackc #:rove)
  :components ((:module "t"
			:components ((:file "package")
				     (:file "util")
				     (:file "tokenizer-test")
				     (:file "analyzer-test")
				     (:file "parser-test")))))
