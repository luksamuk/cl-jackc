;;;; cl-jackc.asd

(asdf:defsystem #:cl-jackc
  :description "Jack compiler for the nand2tetris Hack platform."
  :author "Lucas Vieira <lucasvieira@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:module "src"
			:components ((:file "cl-jackc")
				     (:file "reader")
				     (:file "analyzer")
				     (:file "tokenizer")
				     (:file "parser")
				     (:file "writer")))))
