;;; -*- Mode: Lisp; Package: Xlib; Log: clx.log -*-

;; This file contains some of the system dependent code for CLX

;;;
;;;                      TEXAS INSTRUMENTS INCORPORATED
;;;                               P.O. BOX 2909
;;;                            AUSTIN, TEXAS 78769
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

(proclaim '(declaration array-register))

;;; The size of the output buffer.  Must be a multiple of 4.
(defparameter *output-buffer-size* #x10000)

;;; Number of seconds to wait for a reply to a server request
(defparameter *reply-timeout* nil)

(progn
  (defconstant +word-0+ 0)
  (defconstant +word-1+ 1)

  (defconstant +long-0+ 0)
  (defconstant +long-1+ 1)
  (defconstant +long-2+ 2)
  (defconstant +long-3+ 3))

;;; Set some compiler-options for often used code

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +buffer-speed+ #+clx-debugging 1 #-clx-debugging 3
               "Speed compiler option for buffer code.")
  (defconstant +buffer-safety+ #+clx-debugging 3 #-clx-debugging 1
               "Safety compiler option for buffer code.")
  (defconstant +buffer-debug+ #+clx-debugging 3 #-clx-debugging 1
               "Debug compiler option for buffer code>")
  (defun declare-bufmac ()
    `(declare (optimize
               (speed ,+buffer-speed+)
               (safety ,+buffer-safety+)
               (debug ,+buffer-debug+))))
  ;; It's my impression that in lucid there's some way to make a
  ;; declaration called fast-entry or something that causes a function
  ;; to not do some checking on args. Sadly, we have no lucid manuals
  ;; here.  If such a declaration is available, it would be a good
  ;; idea to make it here when +buffer-speed+ is 3 and +buffer-safety+
  ;; is 0.
  (defun declare-buffun ()
    `(declare (optimize
               (speed ,+buffer-speed+)
               (safety ,+buffer-safety+)
               (debug ,+buffer-debug+)))))

(declaim (inline card8->int8 int8->card8
                 card16->int16 int16->card16
                 card32->int32 int32->card32))

(defun card8->int8 (x)
  (declare (type card8 x))
  (declare (clx-values int8))
  #.(declare-buffun)
  (the int8 (if (logbitp 7 x)
                (the int8 (- x #x100))
                x)))

(defun int8->card8 (x)
  (declare (type int8 x))
  (declare (clx-values card8))
  #.(declare-buffun)
  (the card8 (ldb (byte 8 0) x)))

(defun card16->int16 (x)
  (declare (type card16 x))
  (declare (clx-values int16))
  #.(declare-buffun)
  (the int16 (if (logbitp 15 x)
                 (the int16 (- x #x10000))
                 x)))

(defun int16->card16 (x)
  (declare (type int16 x))
  (declare (clx-values card16))
  #.(declare-buffun)
  (the card16 (ldb (byte 16 0) x)))

(defun card32->int32 (x)
  (declare (type card32 x))
  (declare (clx-values int32))
  #.(declare-buffun)
  (the int32 (if (logbitp 31 x)
                 (the int32 (- x #x100000000))
                 x)))

(defun int32->card32 (x)
  (declare (type int32 x))
  (declare (clx-values card32))
  #.(declare-buffun)
  (the card32 (ldb (byte 32 0) x)))

(declaim (inline aref-card8 aset-card8 aref-int8 aset-int8))

(defun aref-card8 (a i)
  (declare (type buffer-bytes a)
           (type array-index i))
  (declare (clx-values card8))
  #.(declare-buffun)
  (the card8 (aref a i)))

(defun aset-card8 (v a i)
  (declare (type card8 v)
           (type buffer-bytes a)
           (type array-index i))
  #.(declare-buffun)
  (setf (aref a i) v))

(defun aref-int8 (a i)
  (declare (type buffer-bytes a)
           (type array-index i))
  (declare (clx-values int8))
  #.(declare-buffun)
  (card8->int8 (aref a i)))

(defun aset-int8 (v a i)
  (declare (type int8 v)
           (type buffer-bytes a)
           (type array-index i))
  #.(declare-buffun)
  (setf (aref a i) (int8->card8 v)))

(progn
  (defun aref-card16 (a i)
    (declare (type buffer-bytes a)
             (type array-index i))
    (declare (clx-values card16))
    #.(declare-buffun)
    (the card16
         (logior (the card16
                      (ash (the card8 (aref a (index+ i +word-1+))) 8))
                 (the card8
                      (aref a (index+ i +word-0+))))))

  (defun aset-card16 (v a i)
    (declare (type card16 v)
             (type buffer-bytes a)
             (type array-index i))
    #.(declare-buffun)
    (setf (aref a (index+ i +word-1+)) (the card8 (ldb (byte 8 8) v))
          (aref a (index+ i +word-0+)) (the card8 (ldb (byte 8 0) v)))
    v)

  (defun aref-int16 (a i)
    (declare (type buffer-bytes a)
             (type array-index i))
    (declare (clx-values int16))
    #.(declare-buffun)
    (the int16
         (logior (the int16
                      (ash (the int8 (aref-int8 a (index+ i +word-1+))) 8))
                 (the card8
                      (aref a (index+ i +word-0+))))))

  (defun aset-int16 (v a i)
    (declare (type int16 v)
             (type buffer-bytes a)
             (type array-index i))
    #.(declare-buffun)
    (setf (aref a (index+ i +word-1+)) (the card8 (ldb (byte 8 8) v))
          (aref a (index+ i +word-0+)) (the card8 (ldb (byte 8 0) v)))
    v)

  (defun aref-card32 (a i)
    (declare (type buffer-bytes a)
             (type array-index i))
    (declare (clx-values card32))
    #.(declare-buffun)
    (the card32
         (logior (the card32
                      (ash (the card8 (aref a (index+ i +long-3+))) 24))
                 (the card29
                      (ash (the card8 (aref a (index+ i +long-2+))) 16))
                 (the card16
                      (ash (the card8 (aref a (index+ i +long-1+))) 8))
                 (the card8
                      (aref a (index+ i +long-0+))))))

  (defun aset-card32 (v a i)
    (declare (type card32 v)
             (type buffer-bytes a)
             (type array-index i))
    #.(declare-buffun)
    (setf (aref a (index+ i +long-3+)) (the card8 (ldb (byte 8 24) v))
          (aref a (index+ i +long-2+)) (the card8 (ldb (byte 8 16) v))
          (aref a (index+ i +long-1+)) (the card8 (ldb (byte 8 8) v))
          (aref a (index+ i +long-0+)) (the card8 (ldb (byte 8 0) v)))
    v)

  (defun aref-int32 (a i)
    (declare (type buffer-bytes a)
             (type array-index i))
    (declare (clx-values int32))
    #.(declare-buffun)
    (the int32
         (logior (the int32
                      (ash (the int8 (aref-int8 a (index+ i +long-3+))) 24))
                 (the card29
                      (ash (the card8 (aref a (index+ i +long-2+))) 16))
                 (the card16
                      (ash (the card8 (aref a (index+ i +long-1+))) 8))
                 (the card8
                      (aref a (index+ i +long-0+))))))

  (defun aset-int32 (v a i)
    (declare (type int32 v)
             (type buffer-bytes a)
             (type array-index i))
    #.(declare-buffun)
    (setf (aref a (index+ i +long-3+)) (the card8 (ldb (byte 8 24) v))
          (aref a (index+ i +long-2+)) (the card8 (ldb (byte 8 16) v))
          (aref a (index+ i +long-1+)) (the card8 (ldb (byte 8 8) v))
          (aref a (index+ i +long-0+)) (the card8 (ldb (byte 8 0) v)))
    v)

  (defun aref-card29 (a i)
    (declare (type buffer-bytes a)
             (type array-index i))
    (declare (clx-values card29))
    #.(declare-buffun)
    (the card29
         (logior (the card29
                      (ash (the card8 (aref a (index+ i +long-3+))) 24))
                 (the card29
                      (ash (the card8 (aref a (index+ i +long-2+))) 16))
                 (the card16
                      (ash (the card8 (aref a (index+ i +long-1+))) 8))
                 (the card8
                      (aref a (index+ i +long-0+))))))

  (defun aset-card29 (v a i)
    (declare (type card29 v)
             (type buffer-bytes a)
             (type array-index i))
    #.(declare-buffun)
    (setf (aref a (index+ i +long-3+)) (the card8 (ldb (byte 8 24) v))
          (aref a (index+ i +long-2+)) (the card8 (ldb (byte 8 16) v))
          (aref a (index+ i +long-1+)) (the card8 (ldb (byte 8 8) v))
          (aref a (index+ i +long-0+)) (the card8 (ldb (byte 8 0) v)))
    v))

(defsetf aref-card8 (a i) (v)
  `(aset-card8 ,v ,a ,i))

(defsetf aref-int8 (a i) (v)
  `(aset-int8 ,v ,a ,i))

(defsetf aref-card16 (a i) (v)
  `(aset-card16 ,v ,a ,i))

(defsetf aref-int16 (a i) (v)
  `(aset-int16 ,v ,a ,i))

(defsetf aref-card32 (a i) (v)
  `(aset-card32 ,v ,a ,i))

(defsetf aref-int32 (a i) (v)
  `(aset-int32 ,v ,a ,i))

(defsetf aref-card29 (a i) (v)
  `(aset-card29 ,v ,a ,i))

;;; Other random conversions
(defun rgb-val->card16 (value)
  ;; Short floats are good enough
  (declare (type rgb-val value))
  (declare (clx-values card16))
  #.(declare-buffun)
  ;; Convert VALUE from float to card16
  (the card16 (values (round (the rgb-val value) #.(/ 1.0s0 #xffff)))))

(defun card16->rgb-val (value)
  ;; Short floats are good enough
  (declare (type card16 value))
  (declare (clx-values short-float))
  #.(declare-buffun)
  ;; Convert VALUE from card16 to float
  (the short-float (* (the card16 value) #.(/ 1.0s0 #xffff))))

(defun radians->int16 (value)
  ;; Short floats are good enough
  (declare (type angle value))
  (declare (clx-values int16))
  #.(declare-buffun)
  (the int16 (values (round (the angle value) #.(float (/ pi 180.0s0 64.0s0) 0.0s0)))))

(defun int16->radians (value)
  ;; Short floats are good enough
  (declare (type int16 value))
  (declare (clx-values short-float))
  #.(declare-buffun)
  (the short-float (* (the int16 value) #.(coerce (/ pi 180.0 64.0) 'short-float))))


(progn
;;; This overrides the (probably incorrect) definition in clx.lisp.  Since PI
;;; is irrational, there can't be a precise rational representation.  In
;;; particular, the different float approximations will always be /=.  This
;;; causes problems with type checking, because people might compute an
;;; argument in any precision.  What we do is discard all the excess precision
;;; in the value, and see if the protocol encoding falls in the desired range
;;; (64'ths of a degree.)
;;;
  (deftype angle () '(satisfies anglep))

  (defun anglep (x)
    (and (typep x 'real)
         (<= (* -360 64)
             (the int16 (round x #.(float (/ pi 180.0s0 64.0s0) 0.0s0)))
             (* 360 64)))))

;;-----------------------------------------------------------------------------
;; Character transformation
;;-----------------------------------------------------------------------------


;;; This stuff transforms chars to ascii codes in card8's and back.
;;; You might have to hack it a little to get it to work for your machine.

(declaim (inline char->card8 card8->char))

(macrolet ((char-translators ()
             (let ((alist
                    `(#-lispm
                      ;; The normal ascii codes for the control characters.
                      ,@`((#\Return . 13)
                          (#\Linefeed . 10)
                          (#\Rubout . 127)
                          (#\Page . 12)
                          (#\Tab . 9)
                          (#\Backspace . 8)
                          (#\Newline . 10)
                          (#\Space . 32))
                      ;; The rest of the common lisp charater set with the normal
                      ;; ascii codes for them.
                      (#\! . 33) (#\" . 34) (#\# . 35) (#\$ . 36)
                      (#\% . 37) (#\& . 38) (#\' . 39) (#\( . 40)
                      (#\) . 41) (#\* . 42) (#\+ . 43) (#\, . 44)
                      (#\- . 45) (#\. . 46) (#\/ . 47) (#\0 . 48)
                      (#\1 . 49) (#\2 . 50) (#\3 . 51) (#\4 . 52)
                      (#\5 . 53) (#\6 . 54) (#\7 . 55) (#\8 . 56)
                      (#\9 . 57) (#\: . 58) (#\; . 59) (#\< . 60)
                      (#\= . 61) (#\> . 62) (#\? . 63) (#\@ . 64)
                      (#\A . 65) (#\B . 66) (#\C . 67) (#\D . 68)
                      (#\E . 69) (#\F . 70) (#\G . 71) (#\H . 72)
                      (#\I . 73) (#\J . 74) (#\K . 75) (#\L . 76)
                      (#\M . 77) (#\N . 78) (#\O . 79) (#\P . 80)
                      (#\Q . 81) (#\R . 82) (#\S . 83) (#\T . 84)
                      (#\U . 85) (#\V . 86) (#\W . 87) (#\X . 88)
                      (#\Y . 89) (#\Z . 90) (#\[ . 91) (#\\ . 92)
                      (#\] . 93) (#\^ . 94) (#\_ . 95) (#\` . 96)
                      (#\a . 97) (#\b . 98) (#\c . 99) (#\d . 100)
                      (#\e . 101) (#\f . 102) (#\g . 103) (#\h . 104)
                      (#\i . 105) (#\j . 106) (#\k . 107) (#\l . 108)
                      (#\m . 109) (#\n . 110) (#\o . 111) (#\p . 112)
                      (#\q . 113) (#\r . 114) (#\s . 115) (#\t . 116)
                      (#\u . 117) (#\v . 118) (#\w . 119) (#\x . 120)
                      (#\y . 121) (#\z . 122) (#\{ . 123) (#\| . 124)
                      (#\} . 125) (#\~ . 126))))
               (cond ((dolist (pair alist nil)
                        (when (not (= (char-code (car pair)) (cdr pair)))
                          (return t)))
                      `(progn
                         (defconstant *char-to-card8-translation-table*
                           ',(let ((array (make-array
                                           (let ((max-char-code 255))
                                             (dolist (pair alist)
                                               (setq max-char-code
                                                     (max max-char-code
                                                          (char-code (car pair)))))
                                             (1+ max-char-code))
                                           :element-type 'card8)))
                               (dotimes (i (length array))
                                 (setf (aref array i) (mod i 256)))
                               (dolist (pair alist)
                                 (setf (aref array (char-code (car pair)))
                                       (cdr pair)))
                               array))
                         (defconstant *card8-to-char-translation-table*
                           ',(let ((array (make-array 256)))
                               (dotimes (i (length array))
                                 (setf (aref array i) (code-char i)))
                               (dolist (pair alist)
                                 (setf (aref array (cdr pair)) (car pair)))
                               array))
                         (defun char->card8 (char)
                           (declare (type base-char char))
                           #.(declare-buffun)
                           (the card8 (aref (the (simple-array card8 (*))
                                                 *char-to-card8-translation-table*)
                                            (the array-index (char-code char)))))
                         (defun card8->char (card8)
                           (declare (type card8 card8))
                           #.(declare-buffun)
                           (the base-char
                                (or (aref (the simple-vector *card8-to-char-translation-table*)
                                          card8)
                                    (error "Invalid CHAR code ~D." card8))))
                         (dotimes (i 256)
                           (unless (= i (char->card8 (card8->char i)))
                             (warn "The card8->char mapping is not invertible through char->card8.  Info:~%~S"
                                   (list i
                                         (card8->char i)
                                         (char->card8 (card8->char i))))
                             (return nil)))
                         (dotimes (i (length *char-to-card8-translation-table*))
                           (let ((char (code-char i)))
                             (unless (eql char (card8->char (char->card8 char)))
                               (warn "The char->card8 mapping is not invertible through card8->char.  Info:~%~S"
                                     (list char
                                           (char->card8 char)
                                           (card8->char (char->card8 char))))
                               (return nil))))))
                     (t
                      `(progn
                         (defun char->card8 (char)
                           (declare (type base-char char))
                           #.(declare-buffun)
                           (the card8 (char-code char)))
                         (defun card8->char (card8)
                           (declare (type card8 card8))
                           #.(declare-buffun)
                           (the base-char (code-char card8)))))))))
  (char-translators))

;;-----------------------------------------------------------------------------
;; Process Locking
;;
;;	Common-Lisp doesn't provide process locking primitives, so we define
;;	our own here, based on Zetalisp primitives.  Holding-Lock is very
;;	similar to with-lock on The TI Explorer, and a little more efficient
;;	than with-process-lock on a Symbolics.
;;-----------------------------------------------------------------------------

;;; MAKE-PROCESS-LOCK: Creating a process lock.

(defun make-process-lock (name)
  (sb-thread:make-mutex :name name))

(defmacro holding-lock ((lock display &optional (whostate "CLX wait")
                              &key timeout)
                        &body body)
  ;; This macro is used by WITH-DISPLAY, which claims to be callable
  ;; recursively.  So, had better use a recursive lock.
  (declare (ignore display whostate))
  `(sb-thread:with-recursive-lock (,lock ,@(when timeout
                                             `(:timeout ,timeout)))
     ,@body))

;;; WITHOUT-ABORTS

;;; If you can inhibit asynchronous keyboard aborts inside the body of this
;;; macro, then it is a good idea to do this.  This macro is wrapped around
;;; request writing and reply reading to ensure that requests are atomically
;;; written and replies are atomically read from the stream.

(defmacro without-aborts (&body body)
  `(progn ,@body))

;;; PROCESS-BLOCK: Wait until a given predicate returns a non-NIL value.
;;; Caller guarantees that PROCESS-WAKEUP will be called after the predicate's
;;; value changes.

(progn
  (declaim (inline yield))
  (defun yield ()
    (declare (optimize speed (safety 0)))
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "sched_yield" (function sb-alien:int)))
    (values)))

(defun process-block (whostate predicate &rest predicate-args)
  (declare (ignore whostate))
  (declare (type function predicate))
  (loop
     (when (apply predicate predicate-args)
       (return))
     (yield)))

(defun process-block (whostate predicate &rest predicate-args)
  (declare (ignore whostate))
  (declare (type function predicate))
  (loop
     (when (apply predicate predicate-args)
       (return))
    (yield)))

(defun process-block (whostate predicate &rest predicate-args)
  (declare (ignore whostate))
  (declare (type function predicate))
  (loop
   (when (apply predicate predicate-args)
     (return))
     (yield)))

;;; FIXME: the below implementation for threaded PROCESS-BLOCK using
;;; queues and condition variables might seem better, but in fact it
;;; turns out to make performance extremely suboptimal, at least as
;;; measured by McCLIM on linux 2.4 kernels.  -- CSR, 2003-11-10
#+(or)
(defvar *process-conditions* (make-hash-table))

#+(or)
(defun process-block (whostate predicate &rest predicate-args)
  (declare (ignore whostate))
  (declare (type function predicate))
  (let* ((pid (sb-thread:current-thread-id))
         (last (gethash  pid *process-conditions*))
         (lock
          (or (car last)
              (sb-thread:make-mutex :name (format nil "lock ~A" pid))))
         (queue
          (or (cdr last)
              (sb-thread:make-waitqueue :name (format nil "queue ~A" pid)))))
    (unless last
      (setf (gethash pid *process-conditions*) (cons lock queue)))
    (sb-thread:with-mutex (lock)
      (loop
         (when (apply predicate predicate-args) (return))
         (handler-case
             (sb-ext:with-timeout .5
               (sb-thread:condition-wait queue lock))
           (sb-ext:timeout ()
             (format *trace-output* "thread ~A, process-block timed out~%"
                     (sb-thread:current-thread-id) )))))))

;;; PROCESS-WAKEUP: Check some other process' wait function.

(declaim (inline process-wakeup))

(defun process-wakeup (process)
  (declare (ignore process))
  (yield))

;;; CURRENT-PROCESS: Return the current process object for input locking and
;;; for calling PROCESS-WAKEUP.

(declaim (inline current-process))

;;; Default return NIL, which is acceptable even if there is a scheduler.

(defun current-process ()
  sb-thread:*current-thread*)

;;; WITHOUT-INTERRUPTS -- provide for atomic operations.

(defvar *without-interrupts-sic-lock*
  (sb-thread:make-mutex :name "lock simulating *without-interrupts*"))

(defmacro without-interrupts (&body body)
  `(sb-thread:with-recursive-lock (*without-interrupts-sic-lock*)
     ,@body))

;;; CONDITIONAL-STORE:

;; This should use GET-SETF-METHOD to avoid evaluating subforms multiple times.
;; It doesn't because CLtL doesn't pass the environment to GET-SETF-METHOD.

(defmacro conditional-store (place old-value new-value)
  (let ((ov (gensym)))
    `(let ((,ov ,old-value))
       (eq ,ov (sb-ext:compare-and-swap ,place ,ov ,new-value)))))

;;;----------------------------------------------------------------------------
;;; IO Error Recovery
;;;	All I/O operations are done within a WRAP-BUF-OUTPUT macro.
;;;	It prevents multiple mindless errors when the network craters.
;;;
;;;----------------------------------------------------------------------------

(defmacro wrap-buf-output ((buffer) &body body)
  ;; Error recovery wrapper
  `(unless (buffer-dead ,buffer)
     ,@body))

(defmacro wrap-buf-input ((buffer) &body body)
  (declare (ignore buffer))
  ;; Error recovery wrapper
  `(progn ,@body))

;;;----------------------------------------------------------------------------
;;; System dependent IO primitives
;;;	Functions for opening, reading writing forcing-output and closing
;;;	the stream to the server.
;;;----------------------------------------------------------------------------

;;; OPEN-X-STREAM - create a stream for communicating to the appropriate X
;;; server

(defun open-x-stream (host display protocol)
  (declare (ignore protocol)
           (type (integer 0) display))
  (socket-make-stream
   (let ((unix-domain-socket-path (unix-socket-path-from-host host display)))
     (if unix-domain-socket-path
         (let ((s (make-instance 'local-socket :type :stream)))
           (socket-connect s unix-domain-socket-path)
           s)
         (let ((host (car (host-ent-addresses (get-host-by-name host)))))
           (when host
             (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
               (socket-connect s host (+ 6000 display))
               s)))))
   :element-type '(unsigned-byte 8)
   :input t :output t :buffering :none))

;;; BUFFER-INPUT-WAIT-DEFAULT - wait for for input to be available for the
;;; buffer.  This is called in read-input between requests, so that a process
;;; waiting for input is abortable when between requests.  Should return
;;; :TIMEOUT if it times out, NIL otherwise.

(defun buffer-input-wait-default (display timeout)
  (declare (type display display)
           (type (or null number) timeout))
  (let ((stream (display-input-stream display)))
    (declare (type (or null stream) stream))
    (cond ((null stream))
          ((listen stream) nil)
          ((eql timeout 0) :timeout)
          ;; MP package protocol may be shared between clisp and cmu.
          ((or (sb-sys:wait-until-fd-usable (sb-sys:fd-stream-fd stream) :input timeout))
           nil)
          (t :timeout))))

;;;----------------------------------------------------------------------------
;;; System dependent speed hacks
;;;----------------------------------------------------------------------------

;;
;; WITH-STACK-LIST is used by WITH-STATE as a memory saving feature.
;; If your lisp doesn't have stack-lists, and you're worried about
;; consing garbage, you may want to re-write this to allocate and
;; initialize lists from a resource.
;;
(defmacro with-stack-list ((var &rest elements) &body body)
  ;; SYNTAX: (WITH-STACK-LIST (var exp1 ... expN) body)
  ;; Equivalent to (LET ((var (MAPCAR #'EVAL '(exp1 ... expN)))) body)
  ;; except that the list produced by MAPCAR resides on the stack and
  ;; therefore DISAPPEARS when WITH-STACK-LIST is exited.
  `(let ((,var (list ,@elements)))
     (declare (type cons ,var)
              (dynamic-extent ,var))
     ,@body))

(defmacro with-stack-list* ((var &rest elements) &body body)
  ;; SYNTAX: (WITH-STACK-LIST* (var exp1 ... expN) body)
  ;; Equivalent to (LET ((var (APPLY #'LIST* (MAPCAR #'EVAL '(exp1 ... expN))))) body)
  ;; except that the list produced by MAPCAR resides on the stack and
  ;; therefore DISAPPEARS when WITH-STACK-LIST is exited.
  `(let ((,var (list* ,@elements)))
     (declare (type cons ,var)
              (dynamic-extent ,var))
     ,@body))

(declaim (inline buffer-replace))

(defun buffer-replace (buf1 buf2 start1 end1 &optional (start2 0))
  (declare (type buffer-bytes buf1 buf2)
           (type array-index start1 end1 start2))
  (replace buf1 buf2 :start1 start1 :end1 end1 :start2 start2))

(defmacro with-gcontext-bindings ((gc saved-state indexes ts-index temp-mask temp-gc)
                                  &body body)
  (let ((local-state (gensym))
        (resets nil))
    (dolist (index indexes)
      (push `(setf (svref ,local-state ,index) (svref ,saved-state ,index))
            resets))
    `(unwind-protect
          (progn
            ,@body)
       (let ((,local-state (gcontext-local-state ,gc)))
         (declare (type gcontext-state ,local-state))
         ,@resets
         (setf (svref ,local-state ,ts-index) 0))
       (when ,temp-gc
         (restore-gcontext-temp-state ,gc ,temp-mask ,temp-gc))
       (deallocate-gcontext-state ,saved-state))))

;;;----------------------------------------------------------------------------
;;; How much error detection should CLX do?
;;; Several levels are possible:
;;;
;;; 1. Do the equivalent of check-type on every argument.
;;;
;;; 2. Simply report TYPE-ERROR.  This eliminates overhead of all the format
;;;    strings generated by check-type.
;;;
;;; 3. Do error checking only on arguments that are likely to have errors
;;;    (like keyword names)
;;;
;;; 4. Do error checking only where not doing so may dammage the envirnment
;;;    on a non-tagged machine (i.e. when storing into a structure that has
;;;    been passed in)
;;;
;;; 5. No extra error detection code.  On lispm's, ASET may barf trying to
;;;    store a non-integer into a number array.
;;;
;;; How extensive should the error checking be?  For example, if the server
;;; expects a CARD16, is is sufficient for CLX to check for integer, or
;;; should it also check for non-negative and less than 65536?
;;;----------------------------------------------------------------------------

;; The +TYPE-CHECK?+ constant controls how much error checking is done.
;; Possible values are:
;;    NIL      - Don't do any error checking
;;    t        - Do the equivalent of checktype on every argument
;;    :minimal - Do error checking only where errors are likely

;;; This controls macro expansion, and isn't changable at run-time You will
;;; probably want to set this to nil if you want good performance at
;;; production time.

(defconstant +type-check?+
  #+clx-debugging t
  #-clx-debugging nil)

;; TYPE? is used to allow the code to do error checking at a different level from
;; the declarations.  It also does some optimizations for systems that don't have
;; good compiler support for TYPEP.  The definitions for CARD32, CARD16, INT16, etc.
;; include range checks.  You can modify TYPE? to do less extensive checking
;; for these types if you desire.

;;
;; ### This comment is a lie!  TYPE? is really also used for run-time type
;; dispatching, not just type checking.  -- Ram.

(defmacro type? (object type)
  `(typep ,object ,type))

;; X-TYPE-ERROR is the function called for type errors.
;; If you want lots of checking, but are concerned about code size,
;; this can be made into a macro that ignores some parameters.

(defun x-type-error (object type &optional error-string)
  (x-error 'x-type-error
           :datum object
           :expected-type type
           :type-string error-string))

;;-----------------------------------------------------------------------------
;; Error handlers
;;    Hack up KMP error signaling using zetalisp until the real thing comes
;;    along
;;-----------------------------------------------------------------------------

(defun default-error-handler (display error-key &rest key-vals
                              &key asynchronous &allow-other-keys)
  (declare (type generalized-boolean asynchronous)
           (dynamic-extent key-vals))
  ;; The default display-error-handler.
  ;; It signals the conditions listed in the DISPLAY file.
  (if asynchronous
      (apply #'x-cerror "Ignore" error-key :display display :error-key error-key key-vals)
      (apply #'x-error error-key :display display :error-key error-key key-vals)))

(defun x-error (condition &rest keyargs)
  (declare (dynamic-extent keyargs))
  (apply #'error condition keyargs))

(defun x-cerror (proceed-format-string condition &rest keyargs)
  (declare (dynamic-extent keyargs))
  (apply #'cerror proceed-format-string condition keyargs))

;;; X-ERROR for CMU Common Lisp
;;;
;;; We detect a couple condition types for which we disable event handling in
;;; our system.  This prevents going into the debugger or returning to a
;;; command prompt with CLX repeatedly seeing the same condition.  This occurs
;;; because CMU Common Lisp provides for all events (that is, X, input on file
;;; descriptors, Mach messages, etc.) to come through one routine anyone can
;;; use to wait for input.
;;;

(define-condition x-error (error) ())

;;-----------------------------------------------------------------------------
;;  HOST hacking
;;-----------------------------------------------------------------------------

(defun host-address (host &optional (family :internet))
  ;; Return a list whose car is the family keyword (:internet :DECnet :Chaos)
  ;; and cdr is a list of network address bytes.
  (declare (type stringable host)
           (type (or null (member :internet :decnet :chaos) card8) family))
  (declare (clx-values list))
  (let ((hostent (get-host-by-name (string host))))
    (ecase family
      ((:internet nil 0)
       (cons :internet (coerce (host-ent-address hostent) 'list))))))

;;-----------------------------------------------------------------------------
;; Whether to use closures for requests or not.
;;-----------------------------------------------------------------------------

;;; If this macro expands to non-NIL, then request and locking code is
;;; compiled in a much more compact format, as the common code is shared, and
;;; the specific code is built into a closure that is funcalled by the shared
;;; code.  If your compiler makes efficient use of closures then you probably
;;; want to make this expand to T, as it makes the code more compact.

(defmacro use-closures () nil)

;;; fixme: remove no-op
(defun clx-macroexpand (form env)
  (macroexpand form env))

;;-----------------------------------------------------------------------------
;; Resource stuff
;;-----------------------------------------------------------------------------


;;; Utilities

(defun getenv (name)
  (sb-ext:posix-getenv name))

(defun get-host-name ()
  "Return the same hostname as gethostname(3) would"
  ;; machine-instance probably works on a lot of lisps, but clisp is not
  ;; one of them
  (machine-instance))

(defun homedir-file-pathname (name)
  (and (merge-pathnames (user-homedir-pathname) (pathname name))))

;;; DEFAULT-RESOURCES-PATHNAME - The pathname of the resources file to load if
;;; a resource manager isn't running.

(defun default-resources-pathname ()
  (homedir-file-pathname ".Xdefaults"))

;;; RESOURCES-PATHNAME - The pathname of the resources file to load after the
;;; defaults have been loaded.

(defun resources-pathname ()
  (or (let ((string (getenv "XENVIRONMENT")))
        (and string
             (pathname string)))
      (homedir-file-pathname
       (concatenate 'string ".Xdefaults-" (get-host-name)))))

;;; AUTHORITY-PATHNAME - The pathname of the authority file.

(defun authority-pathname ()
  (or (let ((xauthority (getenv "XAUTHORITY")))
        (and xauthority
             (pathname xauthority)))
      (homedir-file-pathname ".Xauthority")))

;;; this particular defaulting behaviour is typical to most Unices, I think
#+unix
(defun get-default-display (&optional display-name)
  "Parse the argument DISPLAY-NAME, or the environment variable $DISPLAY
if it is NIL.  Display names have the format

  [protocol/] [hostname] : [:] displaynumber [.screennumber]

There are two special cases in parsing, to match that done in the Xlib
C language bindings

 - If the hostname is ``unix'' or the empty string, any supplied
   protocol is ignored and a connection is made using the :local
   transport.

 - If a double colon separates hostname from displaynumber, the
   protocol is assumed to be decnet.

Returns a list of (host display-number screen protocol)."
  (let* ((name (or display-name
                   (getenv "DISPLAY")
                   (error "DISPLAY environment variable is not set")))
         (slash-i (or (position #\/ name) -1))
         (colon-i (position #\: name :start (1+ slash-i)))
         (decnet-colon-p (eql (elt name (1+ colon-i)) #\:))
         (host (subseq name (1+ slash-i) (if decnet-colon-p
                                             (1+ colon-i)
                                             colon-i)))
         (dot-i (and colon-i (position #\. name :start colon-i)))
         (display (when colon-i
                    (parse-integer name
                                   :start (if decnet-colon-p
                                              (+ colon-i 2)
                                              (1+ colon-i))
                                   :end dot-i)))
         (screen (when dot-i
                   (parse-integer name :start (1+ dot-i))))
         (protocol
          (cond ((or (string= host "") (string-equal host "unix")) :local)
                (decnet-colon-p :decnet)
                ((> slash-i -1) (intern
                                 (string-upcase (subseq name 0 slash-i))
                                 :keyword))
                (t :internet))))
    (list host (or display 0) (or screen 0) protocol)))

;;-----------------------------------------------------------------------------
;; GC stuff
;;-----------------------------------------------------------------------------

(defun gc-cleanup ()
  (declare (special *event-free-list*
                    *pending-command-free-list*
                    *reply-buffer-free-lists*
                    *gcontext-local-state-cache*
                    *temp-gcontext-cache*))
  (setq *event-free-list* nil)
  (setq *pending-command-free-list* nil)
  (when (boundp '*reply-buffer-free-lists*)
    (fill *reply-buffer-free-lists* nil))
  (setq *gcontext-local-state-cache* nil)
  (setq *temp-gcontext-cache* nil)
  nil)

;;-----------------------------------------------------------------------------
;; DEFAULT-KEYSYM-TRANSLATE
;;-----------------------------------------------------------------------------

;;; If object is a character, char-bits are set from state.
;;;
;;; [the following isn't implemented (should it be?)]
;;; If object is a list, it is an alist with entries:
;;; (base-char [modifiers] [mask-modifiers])
;;; When MODIFIERS are specified, this character translation
;;; will only take effect when the specified modifiers are pressed.
;;; MASK-MODIFIERS can be used to specify a set of modifiers to ignore.
;;; When MASK-MODIFIERS is missing, all other modifiers are ignored.
;;; In ambiguous cases, the most specific translation is used.

(defun default-keysym-translate (display state object)
  (declare (type display display)
           (type card16 state)
           (type t object)
           (ignore display state)
           (clx-values t))
  object)

;;-----------------------------------------------------------------------------
;; Image stuff
;;-----------------------------------------------------------------------------

;;; Types

(deftype pixarray-1-element-type ()
  'bit)

(deftype pixarray-4-element-type ()
  '(unsigned-byte 4))

(deftype pixarray-8-element-type ()
  '(unsigned-byte 8))

(deftype pixarray-16-element-type ()
  '(unsigned-byte 16))

(deftype pixarray-24-element-type ()
  '(unsigned-byte 24))

(deftype pixarray-32-element-type ()
  '(unsigned-byte 32))

(deftype pixarray-1  ()
  '(simple-array
    pixarray-1-element-type (* *)))

(deftype pixarray-4  ()
  '(simple-array
    pixarray-4-element-type (* *)))

(deftype pixarray-8  ()
  '(simple-array
    pixarray-8-element-type (* *)))

(deftype pixarray-16 ()
  '(simple-array
    pixarray-16-element-type (* *)))

(deftype pixarray-24 ()
  '(simple-array
    pixarray-24-element-type (* *)))

(deftype pixarray-32 ()
  '(simple-array pixarray-32-element-type (* *)))

(deftype pixarray ()
  '(or pixarray-1 pixarray-4 pixarray-8 pixarray-16 pixarray-24 pixarray-32))

(deftype bitmap ()
  'pixarray-1)

;;; WITH-UNDERLYING-SIMPLE-VECTOR

;;; We do *NOT* support viewing an array as having a different element type.
;;; Element-type is ignored.
;;;
(defmacro with-underlying-simple-vector
    ((variable element-type pixarray) &body body)
  (declare (ignore element-type))
  `(sb-kernel:with-array-data
       ((,variable ,pixarray) (start) (end))
     (declare (ignore start end))
     ,@body))

;;; These are used to read and write pixels from and to CARD8s.

;;; READ-IMAGE-LOAD-BYTE is used to extract 1 and 4 bit pixels from CARD8s.

(defmacro read-image-load-byte (size position integer)
  (unless +image-bit-lsb-first-p+ (setq position (- 7 position)))
  `(the (unsigned-byte ,size)
        (ldb (byte ,size ,position)
             (the card8 ,integer))))

;;; READ-IMAGE-ASSEMBLE-BYTES is used to build 16, 24 and 32 bit pixels from
;;; the appropriate number of CARD8s.

(defmacro read-image-assemble-bytes (&rest bytes)
  (unless +image-byte-lsb-first-p+ (setq bytes (reverse bytes)))
  (let ((it (first bytes))
        (count 0))
    (dolist (byte (rest bytes))
      (setq it
            `(dpb (the card8 ,byte)
                  (byte 8 ,(incf count 8))
                  (the (unsigned-byte ,count) ,it))))
    `(the (unsigned-byte ,(* (length bytes) 8)) ,it)))

;;; WRITE-IMAGE-LOAD-BYTE is used to extract a CARD8 from a 16, 24 or 32 bit
;;; pixel.

(defmacro write-image-load-byte (position integer integer-size)
  integer-size
  (unless +image-byte-lsb-first-p+ (setq position (- integer-size 8 position)))
  `(the card8
        (ldb (byte 8 ,position)
             (the (unsigned-byte ,integer-size) ,integer))))

;;; WRITE-IMAGE-ASSEMBLE-BYTES is used to build a CARD8 from 1 or 4 bit
;;; pixels.

(defmacro write-image-assemble-bytes (&rest bytes)
  (unless +image-bit-lsb-first-p+ (setq bytes (reverse bytes)))
  (let ((size (floor 8 (length bytes)))
        (it (first bytes))
        (count 0))
    (dolist (byte (rest bytes))
      (setq it `(#-Genera dpb #+Genera sys:%logdpb
                          (the (unsigned-byte ,size) ,byte)
                          (byte ,size ,(incf count size))
                          (the (unsigned-byte ,count) ,it))))
    `(the card8 ,it)))

;;; If you can write fast routines that can read and write pixarrays out of a
;;; buffer-bytes, do it!  It makes the image code a lot faster.  The
;;; FAST-READ-PIXARRAY, FAST-WRITE-PIXARRAY and FAST-COPY-PIXARRAY routines
;;; return T if they can do it, NIL if they can't.

;;; FIXME: though we have some #+sbcl -conditionalized routines in
;;; here, they would appear not to work, and so are commented out in
;;; the the FAST-xxx-PIXARRAY routines themseleves.  Investigate
;;; whether the unoptimized routines are often used, and also whether
;;; speeding them up while maintaining correctness is possible.

;;; FAST-READ-PIXARRAY - fill part of a pixarray from a buffer of card8s

(defun fast-read-pixarray-24 (buffer-bbuf index array x y width height
                              padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
           (type pixarray-24 array)
           (type card16 width height)
           (type array-index index padded-bytes-per-line)
           (type (member 1 4 8 16 24 32) bits-per-pixel)
           (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (with-underlying-simple-vector (vector pixarray-24-element-type array)
      (do* ((start (index+ index
                           (index* y padded-bytes-per-line)
                           (index* x 3))
                   (index+ start padded-bytes-per-line))
            (y 0 (index1+ y)))
           ((index>= y height))
        (declare (type array-index start y))
        (do* ((end (index+ start (index* width 3)))
              (i start (index+ i 3))
              (x (array-row-major-index array y 0) (index1+ x)))
             ((index>= i end))
          (declare (type array-index end i x))
          (setf (aref vector x)
                (read-image-assemble-bytes
                 (aref buffer-bbuf (index+ i 0))
                 (aref buffer-bbuf (index+ i 1))
                 (aref buffer-bbuf (index+ i 2))))))))
  t)

(defun pixarray-element-size (pixarray)
  (let ((eltype (array-element-type pixarray)))
    (cond ((eq eltype 'bit) 1)
          ((and (consp eltype) (eq (first eltype) 'unsigned-byte))
           (second eltype))
          (t
           (error "Invalid pixarray: ~S." pixarray)))))

;;; COPY-BIT-RECT  --  Internal
;;;
;;;    This is the classic BITBLT operation, copying a rectangular subarray
;;; from one array to another (but source and destination must not overlap.)
;;; Widths are specified in bits.  Neither array can have a non-zero
;;; displacement.  We allow extra random bit-offset to be thrown into the X.
;;;
(defun copy-bit-rect (source source-width sx sy dest dest-width dx dy
                      height width)
  (declare (type array-index source-width sx sy dest-width dx dy height width))
  #.(declare-buffun)
  (sb-kernel:with-array-data ((sdata source) (sstart) (send))
    (declare (ignore send))
    (sb-kernel:with-array-data ((ddata dest) (dstart) (dend))
      (declare (ignore dend))
      (assert (and (zerop sstart) (zerop dstart)))
      (do ((src-idx (index+ (* sb-vm:vector-data-offset sb-vm:n-word-bits)
                            sx (index* sy source-width))
                    (index+ src-idx source-width))
           (dest-idx (index+ (* sb-vm:vector-data-offset sb-vm:n-word-bits)
                             dx (index* dy dest-width))
                     (index+ dest-idx dest-width))
           (count height (1- count)))
          ((zerop count))
        (declare (type array-index src-idx dest-idx count))
        (sb-kernel:ub1-bash-copy sdata src-idx ddata dest-idx width)))))

(defun fast-read-pixarray-using-bitblt
    (bbuf boffset pixarray x y width height padded-bytes-per-line
     bits-per-pixel)
  (declare (type (array * 2) pixarray))
  #.(declare-buffun)
  (copy-bit-rect bbuf
                 (index* padded-bytes-per-line sb-vm:n-byte-bits)
                 (index* boffset sb-vm:n-byte-bits) 0
                 pixarray
                 (index* (array-dimension pixarray 1) bits-per-pixel)
                 x y
                 height
                 (index* width bits-per-pixel))
  t)

(defun fast-read-pixarray (bbuf boffset pixarray
                           x y width height padded-bytes-per-line
                           bits-per-pixel
                           unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
           (type array-index boffset
                 padded-bytes-per-line)
           (type pixarray pixarray)
           (type card16 x y width height)
           (type (member 1 4 8 16 24 32) bits-per-pixel)
           (type (member 8 16 32) unit)
           (type generalized-boolean byte-lsb-first-p bit-lsb-first-p))
  (progn bbuf boffset pixarray x y width height padded-bytes-per-line
         bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
  nil)

;;; FAST-WRITE-PIXARRAY - copy part of a pixarray into an array of CARD8s

(defun fast-write-pixarray-24 (buffer-bbuf index array x y width height
                               padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
           (type pixarray-24 array)
           (type int16 x y)
           (type card16 width height)
           (type array-index index padded-bytes-per-line)
           (type (member 1 4 8 16 24 32) bits-per-pixel)
           (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (with-underlying-simple-vector (vector pixarray-24-element-type array)
      (do* ((h 0 (index1+ h))
            (y y (index1+ y))
            (start index (index+ start padded-bytes-per-line)))
           ((index>= h height))
        (declare (type array-index y start))
        (do* ((end (index+ start (index* width 3)))
              (i start (index+ i 3))
              (x (array-row-major-index array y x) (index1+ x)))
             ((index>= i end))
          (declare (type array-index end i x))
          (let ((pixel (aref vector x)))
            (declare (type pixarray-24-element-type pixel))
            (setf (aref buffer-bbuf (index+ i 0))
                  (write-image-load-byte 0 pixel 24))
            (setf (aref buffer-bbuf (index+ i 1))
                  (write-image-load-byte 8 pixel 24))
            (setf (aref buffer-bbuf (index+ i 2))
                  (write-image-load-byte 16 pixel 24)))))))
  t)

(defun fast-write-pixarray-using-bitblt
    (bbuf boffset pixarray x y width height padded-bytes-per-line
     bits-per-pixel)
  #.(declare-buffun)
  (copy-bit-rect pixarray
                 (index* (array-dimension pixarray 1) bits-per-pixel)
                 x y
                 bbuf
                 (index* padded-bytes-per-line #+cmu vm:byte-bits #+sbcl sb-vm:n-byte-bits)
                 (index* boffset #+cmu vm:byte-bits #+sbcl sb-vm:n-byte-bits) 0
                 height
                 (index* width bits-per-pixel))
  t)

(defun fast-write-pixarray (bbuf boffset pixarray x y width height
                            padded-bytes-per-line bits-per-pixel
                            unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
           (type pixarray pixarray)
           (type card16 x y width height)
           (type array-index boffset padded-bytes-per-line)
           (type (member 1 4 8 16 24 32) bits-per-pixel)
           (type (member 8 16 32) unit)
           (type generalized-boolean byte-lsb-first-p bit-lsb-first-p))
  (progn bbuf boffset pixarray x y width height padded-bytes-per-line
         bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
  nil)

;;; FAST-COPY-PIXARRAY - copy part of a pixarray into another

(defun fast-copy-pixarray (pixarray copy x y width height bits-per-pixel)
  (declare (type pixarray pixarray copy)
           (type card16 x y width height)
           (type (member 1 4 8 16 24 32) bits-per-pixel))
  (progn pixarray copy x y width height bits-per-pixel nil))
