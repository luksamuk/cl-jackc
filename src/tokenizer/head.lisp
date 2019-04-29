;;;; head.lisp
;;;; Part of cl-jackc.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package #:jackc-tokenizer)

;;; Variables

(defvar *integer-constant-symbols*
  (mapcar (lambda (x)
	    (car (coerce x 'list)))
	  (cdar (grammar-lookup :symbol)))
  "Represents symbols which may come directly after integer constants,
excluding whitespace.")

(defvar *string-constant-white-chars* '(#\Newline #\Linefeed #\Return)
  "Enumerates all whitespace characters which may abruptly end a string
constant.")

(defvar *identifier-match-end-chars*
  (append '(#\Space #\Newline #\Tab #\Linefeed #\Return)
	  *integer-constant-symbols*)
  "Enumerates all characters which may end an arbitrary identifier.")

(defvar *identifier-forbidden-matches*
  (append (cdar (grammar-lookup :symbol))
	  (cdar (grammar-lookup :keyword)))
  "Enumerates all tokens which cannot be an identifier")

(defvar *the-head* nil
  "Names a global TOKENIZER-HEAD which may be accessible externally. However,
this variable should not be manipulated directly; instead, one should use the
WITH-NEW-HEAD macro to shadow it appropriately.")


;;; Classes and generics

(defclass tokenizer-head ()
  ((%stream :initarg  :file-stream
	    :initform nil
	    :reader   fstream
	    :documentation "A stream to the head-operated file.")
   (%colnum :initform 0
	    :accessor column-number
	    :documentation "Current column number of the file.")
   (%oldcol :initform nil
	    :accessor old-columns
	    :documentation "Stack of column numbers for each past line.")
   (%linum  :initform 0
	    :accessor line-number
	    :documentation "Current line number of the file."))
  (:documentation "Represents a head which may move forward or backward on a
file stream, and may also hold information regarding its position on the stream."))

(defgeneric head-match (head target)
  (:documentation "Tests whether the HEAD currently points to a TARGET, which
may be a specific token or token type. If so, the head is moved to the end of
the token, and the matching token is returned. If not, returns NIL."))

(defgeneric head-next-char (head)
  (:documentation "Returns the next character at the HEAD file stream, and points
the HEAD to the next character to be fetched."))

(defgeneric head-push-char (head character)
  (:documentation "Pushes a previously fetched CHARACTER to the file stream, prior
to where the HEAD points to, and also rewinds the HEAD to such location."))

(defgeneric head-in-token-p (head token)
  (:documentation "Tests whether the HEAD is currently in the beginning of a
TOKEN. If so, places the HEAD directly after said TOKEN on the stream."))

(defgeneric head-skip-to-end-of-comment (head)
  (:documentation "Fast-forwards the HEAD to the end of a comment, if the HEAD
currently points to the beginning of a comment string."))

(defgeneric head-next-nonseparator (head)
  (:documentation "Returns the next valid character on the file stream held by the
HEAD. The character is guaranteed to be non-whitespace, and to be outside any
comment and different than any comment token."))

(defgeneric head-position (head)
  (:documentation "Returns the current position of the HEAD, with respect to the
beginning of its file stream."))

(defgeneric (setf head-position) (pos head)
  (:documentation "Changes the current position of the HEAD to POS, with respect
to the beginning of its file stream. This does not correct the line and column
number currently held by the HEAD."))

(defgeneric head-ff-nonseparator (head)
  (:documentation "Fast-forwards the HEAD to a non-whitespace character."))


;;; Macros

(defmacro head-checkpoint ((head) &body body)
  "Saves the current position, line and column numbers of HEAD, then evaluates
the BODY. If the BODY returns NIL, the position, line and column numbers of
HEAD are restored."
  (let ((init-position (gensym))
	(line-and-col (gensym))
	(result (gensym)))
    `(let ((,init-position (head-position ,head))
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


;;; Method implementations

(defmethod head-next-char ((head tokenizer-head))
  (let ((charact (read-char (fstream head) nil :eof)))
    (when (eq charact :eof)
      (unexpected-eof-condition))
    (incf (column-number head))
    (when (char= charact #\Newline)
      (incf (line-number head))
      (push (column-number head) (old-columns head))
      (setf (column-number head) 0))
    charact))

(defmethod head-push-char ((head tokenizer-head) char)
  (if (char= char #\Newline)
      (progn
	(decf (line-number head))
	(setf (column-number head) (pop (old-columns head))))
      (decf (column-number head)))
  (unread-char char (fstream head)))

(defmethod head-in-token-p ((head tokenizer-head) token)
  (head-checkpoint (head)
    (loop for token-char across token
       for current = (head-next-char head)
       always (char= token-char current))))

(defmethod head-skip-to-end-of-comment ((head tokenizer-head))
  (loop for comment-pair in *comment-tokens*
     for comment-end = (or (cdr comment-pair) (format nil "~%"))
     if (head-in-token-p head (car comment-pair))
     do (loop until (head-in-token-p head comment-end)
	   do (head-next-char head))))

(defmethod head-next-nonseparator ((head tokenizer-head))
  (let ((current nil))
    (loop while (whitespace-p
		 (progn (head-skip-to-end-of-comment head)
			(setf current (head-next-char head))
			current)))
    current))


(defmethod head-position ((head tokenizer-head))
  (file-position (fstream head)))

(defmethod (setf head-position) (pos (head tokenizer-head))
  (file-position (fstream head) pos))

(defmethod head-ff-nonseparator ((head tokenizer-head))
  (head-push-char head (head-next-nonseparator head)))


(defmethod head-match ((head tokenizer-head) (string string))
  "Tests whether the HEAD currently points to the beginning of a specific
STRING. If so, points the HEAD to the character immediately after the
matching STRING, and returns the matched STRING. If not, returns NIL."
  (when (head-checkpoint (head)
	  (loop for character across string
	     for stream-char = (head-next-nonseparator head)
	     always (char= character stream-char)))
    string))

(defmethod head-match ((head tokenizer-head) (target (eql :integer-constant)))
  "Tests whether the HEAD currently points to the beginning of an integer
constant. If so, points the HEAD to the character immediately after the
matching integer constant, and returns the matched constant as a numeric
value. If not, returns NIL."
  (head-ff-nonseparator head)
  (let ((int-list nil)
	(buffer nil))
    (head-checkpoint (head)
      (loop while t
	 do (setf buffer (head-next-char head))
	 if (or (whitespace-p buffer)
		(member buffer *integer-constant-symbols*))
	 do (progn (head-push-char head buffer)
		   (return))
	 else if (numeric-char-p buffer)
	 do (push buffer int-list))
      (> (list-length int-list) 0))
    (unless (null int-list)
      (let ((integer-value
	     (parse-integer (coerce (reverse int-list) 'string))))
	(if (<= integer-value 32767)
	    integer-value
	    (integer-overflow-condition (line-number head)
					(column-number head)
					integer-value))))))

(defmethod head-match ((head tokenizer-head) (target (eql :string-constant)))
  "Tests whether the HEAD currently points to the beginning of a string
constant (starting and ending with quotes). If so, points the HEAD to the
character immediately after the matching string constant's closing end quote
character, and returns the matched string constant, enclosed in quotes. If
not, returns NIL."
  (head-ff-nonseparator head)
  (let ((str-list nil)
	(has-syntax-error nil))
    (head-checkpoint (head)
      (let ((buffer (head-next-char head)))
	(if (not (char= buffer #\"))
	    (setf has-syntax-error t)
	    (loop while t
	       do (setf buffer (head-next-char head))
	       if (member buffer *string-constant-white-chars*)
	       do (progn (setf has-syntax-error t)
			 (return))
	       else if (char= buffer #\")
	       do (return)
	       else do (push buffer str-list))))
      (not has-syntax-error))
    (unless has-syntax-error
      (coerce (cons #\" (reverse (cons #\" str-list)))
	      'string))))

(defmethod head-match ((head tokenizer-head) (target (eql :identifier)))
  "Tests whether the HEAD currently points to the beginning of an
arbitrary identifier (any token not starting with a number). If so,
points the HEAD to the character immediately after the matching
identifier, and returns the matched identifier. If not, returns NIL."
  (head-ff-nonseparator head)
  (let ((ident-list nil)
	(has-syntax-error nil))
    (head-checkpoint (head)
      (let ((buffer (head-next-char head)))
	(if (or (numeric-char-p buffer)
		(member buffer *identifier-match-end-chars*))
	    (setf has-syntax-error t)
	    (progn (setf ident-list (append ident-list
					    (list buffer)))
		   (loop while t
		      do (setf buffer (head-next-char head))
		      if (member buffer *identifier-match-end-chars*)
		      do (progn (head-push-char head buffer)
				(return))
		      else do (setf ident-list (append ident-list
						       (list buffer)))))))
      (not has-syntax-error))
    (unless has-syntax-error
      (let ((the-identifier (coerce ident-list 'string)))
	(if (member the-identifier
		    *identifier-forbidden-matches*
		    :test #'string=)
	    nil
	    the-identifier)))))


;;; External head-related utilities

(defmacro with-new-head ((file-stream) &body body)
  "Names a scope where the variable *THE-HEAD* is a valid TOKENIZER-HEAD, and
is accessible to the whole BODY. FILE-STREAM should be a newly-opened stream
to the file to be manipulated. The FILE-STREAM is closed after the BODY
execution. The BODY resulting value is preserved."
  (let ((result (gensym)))
    `(let* ((*the-head*
	     (make-instance 'tokenizer-head
			    :file-stream ,file-stream))
	    (,result (progn ,@body)))
       (close (fstream *the-head*))
       ,result)))

