(ql:quickload :cl-jackc/test)
(in-package :cl-jackc/test)
(rove:run :cl-jackc/test)
(unless (first rove/core/suite:*last-suite-report*)
  (uiop:quit -1))

