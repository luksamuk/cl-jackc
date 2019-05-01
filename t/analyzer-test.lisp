;;;; t/analyzer-test.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:cl-jackc/test)

(deftest analyzer-endurance-test
  (write-analyzer-test-cases
   ;; Basic files
   "simple.jack"
   "valid.jack"
   "valid2.jack"
   ;; Minesweeper files
   "Board.jack"  "Random.jack" "Main.jack"
   ;; Book files
   "book/ArrayTest/Main.jack"
   "book/ExpressionLessSquare/Main.jack"
   "book/ExpressionLessSquare/SquareGame.jack"
   "book/ExpressionLessSquare/Square.jack"
   "book/Square/Main.jack"
   "book/Square/SquareGame.jack"
   "book/Square/Square.jack"))
