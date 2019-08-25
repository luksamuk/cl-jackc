;; should be invoked from project root!

(ql:quickload :staple)

(defparameter *output-dir*
  (merge-pathnames "doc/"
		   (asdf:system-source-directory :cl-jackc)))

;; Staple annoyingly generates file-exists errors when we try to
;; generate documentation. Just make sure it supersedes the file
;; when it needs to.
(handler-bind ((file-exists (lambda (c)
			      (declare (ignore c))
			      (format t "Warning: overwriting file~%")
			      (invoke-restart 'supersede))))
  (staple:generate :cl-jackc :output-directory *output-dir*))
  

