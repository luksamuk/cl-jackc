;;;; head.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-tokenizer)

(defclass tokenizer-head ()
  ((%stream :initarg  :file-stream
	    :initform nil
	    :reader   fstream)
   (%colnum :initform 0
	    :accessor column-number)
   (%linum  :initform 0
	    :accessor line-number)))

(defgeneric head-match (head target))
(defgeneric head-next-char (head))
(defgeneric head-next-nonseparator (head))
(defgeneric head-position (head))
(defgeneric (setf head-position) (pos head))
(defgeneric head-ff-nonseparator (head))

(defmethod head-next-char ((head tokenizer-head))
  (let ((charact (read-char (fstream head))))
    (incf (column-number head))
    (when (char= charact #\Newline)
      (incf (line-number head))
      (setf (column-number head) 0))
    charact))

(defun whitespace-p (character)
  (not (null
	(member character
		'(#\Space #\Newline #\Tab #\Linefeed #\Return)))))

(defmethod head-next-nonseparator ((head tokenizer-head))
  (let ((current (head-next-char head)))
    (loop while (whitespace-p current)
       do (setf current (head-next-char head)))
    current))

(defmethod head-position ((head tokenizer-head))
  (file-position (fstream head)))

(defmethod (setf head-position) (pos (head tokenizer-head))
  (file-position (fstream head) pos))

(defmethod head-ff-nonseparator ((head tokenizer-head))
  (head-next-nonseparator head)
  (setf (head-position head) (1- (head-position head))))

(defmacro head-checkpoint ((head) &body body)
  (let ((init-position (gensym))
	(line-and-col (gensym))
	(result (gensym)))
    `(let* ((,init-position (head-position ,head))
	    (,line-and-col (cons (line-number ,head)
				 (column-number ,head)))
	    (,result (progn ,@body)))
       (if (not ,result)
	   (progn
	     (setf (head-position ,head) ,init-position
		   (line-number ,head)   (car ,line-and-col)
		   (column-number ,head) (cdr ,line-and-col))
	     nil)
	   ,result))))

;; Match for string
(defmethod head-match ((head tokenizer-head) (string string))
  (when (head-checkpoint (head)
	  (loop for character across string
	     for stream-char = (head-next-nonseparator head)
	     always (char= character stream-char)))
    string))

(defun numeric-char-p (char)
  (and (>= (char-code char) (char-code #\0))
       (<= (char-code char) (char-code #\9))))

;; Match for numeric constant

(defvar *integer-constant-symbols* (cdar (grammar-lookup :symbol)))

(defmethod head-match ((head tokenizer-head) (target (eql :integer-constant)))
  (head-ff-nonseparator head)
  (let ((int-list nil))
    (head-checkpoint (head)
      (loop while t
	 for buffer = (head-next-char head)
	 if (or (whitespace-p buffer)
		(member buffer *integer-constant-symbols*))
	 do (progn (setf (head-position head)
			 (- (head-position head) 2))
		   (return))
	 else if (numeric-char-p buffer)
	 do (push buffer int-list))
      (> (list-length int-list) 0))
    (unless (null int-list)
      (let ((integer-value
	     (parse-integer (coerce (reverse int-list) 'string))))
	(when (<= integer-value 32767)
	  integer-value)))))

(defvar *string-constant-white-chars* '(#\Newline #\Linefeed #\Return))

(defmethod head-match ((head tokenizer-head) (target (eql :string-constant)))
  (head-ff-nonseparator head)
  (let ((str-list nil)
	(syntax-error nil))
    (head-checkpoint (head)
      (let ((buffer (head-next-char head)))
	(if (not (char= buffer #\"))
	    (setf syntax-error t)
	    (loop while t
	       do (setf buffer (head-next-char head))
	       if (member buffer *string-constant-white-chars*)
	       do (progn (setf syntax-error t)
			 (return))
	       else if (char= buffer #\")
	       do (return)
	       else do (push buffer str-list))))
      (not syntax-error))
    (unless syntax-error
      (coerce (cons #\" (reverse (cons #\" str-list)))
	      'string))))

(defvar *identifier-match-end-chars*
  (append '(#\Space #\Newline #\Tab #\Linefeed #\Return)
	  (mapcar (lambda (x)
		    (car (coerce x 'list)))
		  *integer-constant-symbols*)))

(defmethod head-match ((head tokenizer-head) (target (eql :identifier)))
  (head-ff-nonseparator head)
  (let ((ident-list nil)
	(syntax-error nil))
    (head-checkpoint (head)
      (let ((buffer (head-next-char head)))
	(if (numeric-char-p buffer)
	    (setf syntax-error t)
	    (progn (setf ident-list (append ident-list
					    (list buffer)))
		   (loop while t
		      do (setf buffer (head-next-char head))
		      if (member buffer *identifier-match-end-chars*)
		      do (progn (setf (head-position head)
				      (1- (head-position head)))
				(return))
		      else do (setf ident-list (append ident-list
						       (list buffer)))))))
      (not syntax-error))
    (unless syntax-error
      (coerce ident-list 'string))))
		 



(defvar *the-head* nil)

(defmacro with-new-head ((file-stream) &body body)
  (let ((result (gensym)))
    `(let* ((*the-head*
	     (make-instance 'tokenizer-head
			    :file-stream ,file-stream))
	    (,result (progn ,@body)))
       (close (fstream *the-head*))
       ,result)))

