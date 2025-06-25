;;; This file contains code moved from "dependent" files that has been unified.

(in-package :xlib)

;;; BUFFER-READ-DEFAULT - read data from the X stream
;;;
;;; READ-SEQUENCE was not present in ANSI Common Lisp when CLX was written. This
;;; implementation is portable and implements block transfer.

(defun buffer-read-default (display vector start end timeout)
  (declare (type display display)
           (type buffer-bytes vector)
           (type array-index start end)
           (type (or null (real 0 *)) timeout))
  #.(declare-buffun)
  (if (and (not (null timeout))
           (zerop timeout)
           (not (listen (display-input-stream display))))
      :timeout
      (let ((n (read-sequence vector
                              (display-input-stream display)
                              :start start
                              :end end)))
        (cond
          ((= n end) nil)
          ((= n start) :end-of-file)
          (t :truncated)))))

;;; BUFFER-WRITE-DEFAULT - write data to the X stream
;;;
;;; WRITE-SEQUENCE was not present in ANSI Common Lisp when CLX was
;;; written. This implementation is portable and implements block transfer.
(defun buffer-write-default (vector display start end)
  (declare (type buffer-bytes vector)
	   (type display display)
	   (type array-index start end))
  #.(declare-buffun)

  (write-sequence vector (display-output-stream display) :start start :end end)
  nil)

;;; BUFFER-FORCE-OUTPUT-DEFAULT - force output to the X stream

(defun buffer-force-output-default (display)
  ;; The default buffer force-output function for use with common-lisp streams
  (declare (type display display))
  (let ((stream (display-output-stream display)))
    (declare (type (or null stream) stream))
    (unless (null stream)
      (force-output stream))))

;;; BUFFER-CLOSE-DEFAULT - close the X stream

(defun buffer-close-default (display &key abort)
  ;; The default buffer close function for use with common-lisp streams
  (declare (type display display))
  #.(declare-buffun)
  (let ((stream (display-output-stream display)))
    (declare (type (or null stream) stream))
    (unless (null stream)
      (close stream :abort abort))))

;;; BUFFER-LISTEN-DEFAULT - returns T if there is input available for the
;;; buffer. This should never block, so it can be called from the scheduler.

;;; The default implementation is to just use listen.
(defun buffer-listen-default (display)
  (declare (type display display))
  (let ((stream (display-input-stream display)))
    (declare (type (or null stream) stream))
    (if (null stream)
        t
        (listen stream))))
