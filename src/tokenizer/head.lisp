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
    (when (char= charact #\Newline)
      (incf (line-number head)))
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
	(result (gensym)))
    `(let* ((,init-position (head-position ,head))
	    (,result (progn ,@body)))
       (if (not ,result)
	   (progn
	     (setf (head-position ,head) ,init-position)
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
(defmethod head-match ((head tokenizer-head) (target (eql :integer-constant)))
  (head-ff-nonseparator head)
  (let ((int-list nil)
	(symbols (cdar (grammar-lookup :symbol))))
    (head-checkpoint (head)
      (loop while t
	 for buffer = (head-next-char head)
	 if (or (whitespace-p buffer)
		(member buffer symbols))
	 do (progn (setf (head-position head)
			 (- (head-position head) 2))
		   (return))
	 else if (numeric-char-p buffer)
	 do (setf int-list (append int-list (list buffer))))
      (> (list-length int-list) 0))
    (unless (null int-list)
      ;; TODO: A number > 32767 should be syntax error
      (parse-integer (coerce int-list 'string)))))

(defmethod head-match ((head tokenizer-head) (target (eql :string-constant)))
  )

(defmethod head-match ((head tokenizer-head) (target (eql :identifier)))
  )



(defvar *the-head* nil)

(defmacro with-new-head ((file-stream) &body body)
  (let ((result (gensym)))
    `(let* ((*the-head*
	     (make-instance 'tokenizer-head
			    :file-stream ,file-stream))
	    (,result (progn ,@body)))
       (close (fstream *the-head*))
       ,result)))

