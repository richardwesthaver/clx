;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;; This file contains macro definitions for the BUFFER object for Common-Lisp
;;; X windows version 11

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package :xlib)

;;; The read- macros are in buffer.lisp, because event-case depends on (most of) them.

(defmacro write-card8 (byte-index item)
  `(aset-card8 (the card8 ,item) buffer-bbuf (index+ buffer-boffset ,byte-index)))

(defmacro write-int8 (byte-index item)
  `(aset-int8 (the int8 ,item) buffer-bbuf (index+ buffer-boffset ,byte-index)))

(defmacro write-card16 (byte-index item)
  `(aset-card16 (the card16 ,item) buffer-bbuf
		(index+ buffer-boffset ,byte-index)))

(defmacro write-int16 (byte-index item)
  `(aset-int16 (the int16 ,item) buffer-bbuf
	       (index+ buffer-boffset ,byte-index)))

(defmacro write-card32 (byte-index item)
  `(aset-card32 (the card32 ,item) buffer-bbuf
		(index+ buffer-boffset ,byte-index)))

(defmacro write-int32 (byte-index item)
  `(aset-int32 (the int32 ,item) buffer-bbuf
	       (index+ buffer-boffset ,byte-index)))

(defmacro write-card29 (byte-index item)
  `(aset-card29 (the card29 ,item) buffer-bbuf
		(index+ buffer-boffset ,byte-index)))

;; This is used for 2-byte characters, which may not be aligned on 2-byte boundaries
;; and always are written high-order byte first.
(defmacro write-char2b (byte-index item)
  ;; It is impossible to do an overlapping write, so only nonoverlapping here.
  `(let ((%item ,item)
	 (%byte-index (index+ buffer-boffset ,byte-index)))
     (declare (type card16 %item)
	      (type array-index %byte-index))
     (aset-card8 (the card8 (ldb (byte 8 8) %item)) buffer-bbuf %byte-index)
     (aset-card8 (the card8 (ldb (byte 8 0) %item)) buffer-bbuf (index+ %byte-index 1))))

(defmacro set-buffer-offset (value &environment env)
  env
  `(let ((.boffset. ,value))
     (declare (type array-index .boffset.))
     (setq buffer-boffset .boffset.)))

(defmacro advance-buffer-offset (value)
  `(set-buffer-offset (index+ buffer-boffset ,value)))

(defmacro with-buffer-output ((buffer &key (sizes '(8 16 32)) length index) &body body)
  (unless (listp sizes) (setq sizes (list sizes)))
  `(let ((%buffer ,buffer))
     (declare (type display %buffer))
     ,(declare-bufmac)
     ,(when length
	`(when (index>= (index+ (buffer-boffset %buffer) ,length) (buffer-size %buffer))
	   (buffer-flush %buffer)))
     (let* ((buffer-boffset (the array-index ,(or index `(buffer-boffset %buffer))))
	    (buffer-bbuf (buffer-obuf8 %buffer)))
       (declare (type array-index buffer-boffset))
       (declare (type buffer-bytes buffer-bbuf))
       buffer-boffset
       buffer-bbuf
       ,@body)))

;;; This macro is just used internally in buffer

(defmacro writing-buffer-chunks (type args decls &body body)
  (when (> (length body) 2)
    (error "writing-buffer-chunks called with too many forms"))
  (let* ((size (* 8 (index-increment type)))
	 (form (first body)))
    `(with-buffer-output (buffer :index boffset :sizes ,(reverse (adjoin size '(8))))
       ;; Loop filling the buffer
       (do* (,@args
	     ;; Number of bytes needed to output
	     (len ,(if (= size 8)
		       `(index- end start)
		       `(index-ash (index- end start) ,(truncate size 16)))
		  (index- len chunk))
	     ;; Number of bytes available in buffer
	     (chunk (index-min len (index- (buffer-size buffer) buffer-boffset))
		    (index-min len (index- (buffer-size buffer) buffer-boffset))))
	    ((not (index-plusp len)))
	 (declare ,@decls
		  (type array-index len chunk))
	 ,form
	 (index-incf buffer-boffset chunk)
	 ;; Flush the buffer
	 (when (and (index-plusp len) (index>= buffer-boffset (buffer-size buffer)))
	   (setf (buffer-boffset buffer) buffer-boffset)
	   (buffer-flush buffer)
	   (setq buffer-boffset (buffer-boffset buffer))))
       (setf (buffer-boffset buffer) (lround buffer-boffset))))) 
