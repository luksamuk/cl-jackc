;;;; utils.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-utils)

(defmacro if-let (binding consequent alternative)
  "Binds a BINDING. If the bound symbol has non-false value, executes
CONSEQUENT; otherwise, executes ALTERNATIVE."
  (destructuring-bind ((sym val)) binding
    `(let ((,sym ,val))
       (if ,sym
	   ,consequent
	   ,alternative))))

