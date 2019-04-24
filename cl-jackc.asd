;;;; cl-jackc.asd
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(asdf:defsystem #:cl-jackc
  :description "Jack compiler for the nand2tetris Hack platform."
  :author "Lucas Vieira <lucasvieira@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:split-sequence)
  :components ((:file "package")
               (:module "src"
			:components ((:file "utils")
				     (:file "conditions")
				     (:file "cl-jackc")
				     (:file "reader")
				     (:file "analyzer")
				     (:module "tokenizer"
					      :components ((:file "grammar")
							   (:file "head")
							   (:file "matcher")))
				     (:file "parser")
				     (:file "writer")))))

(asdf:defsystem #:cl-jackc/test
  :description "Test system for cl-jackc Jack compiler"
  :author "Lucas Vieira <lucasvieira@protonmail.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-jackc #:rove)
  :components ((:module "t"
			:components ((:file "test")))))
