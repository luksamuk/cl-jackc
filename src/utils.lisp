;;;; utils.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-utils)

(defun whitespace-p (character)
  "Checks whether CHARACTER is a whitespace character."
  (not (null
	(member character
		'(#\Space #\Newline #\Tab #\Linefeed #\Return)))))

(defun numeric-char-p (char)
  "Checks whether CHAR is a numeric (0~9) character."
  (and (>= (char-code char) (char-code #\0))
       (<= (char-code char) (char-code #\9))))
