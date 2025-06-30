;;; xembed.lisp --- XEmbed Protocol

;; 

;;; Code:
(defpackage :xlib/xembed
  (:nicknames :xembed)
  (:shadowing-import-from :xlib :draw-line :array-index)
  (:use #:cl #:asdf #:xlib :std)
  (:shadow :xor)
  (:export #:dformat #:rformat #:curry #:move-next-to
	   #:window-resize #:window-parent #:handler-pos #:handler-vector #:combine-handlers
	   #:decode-xembed-message-type  #:update-timestamp  #:*timestamp*
	   #:create-socket #:destroy-socket #:client #:embed #:socket-activate #:socket-resize
	   #:socket-list-handler-vector))

(in-package :xlib/xembed)
(define-extension "XEMBED")

;;; Debug
(defparameter *debug-stream* *standard-output*)
(defparameter *debug-level* 0)
(defparameter *show-progress* t)
(defparameter *progress-stream* *standard-output*)
(defparameter *result-stream* *standard-output*)
(defun format-in-or-out (in-or-out)
  (case in-or-out
    (:in ">>>")
    (:out "<<<")
    (otherwise "")))
(defun format-call (fn in-or-out &rest arguments)
  (format nil "~a ~a~:[()~;~:*~S~]" (format-in-or-out in-or-out) fn arguments))

(defun dformat-call (level fn in-or-out &rest arguments)
  (dformat level "~a~%" (apply #'format-call fn in-or-out arguments)))
(defun dformat (level control-string &rest format-arguments)
  (when (<= level *debug-level*)
    (apply #'format *debug-stream* control-string format-arguments)))
(defun pformat (control-string &rest format-arguments)
  (when *show-progress*
    (apply #'format *progress-stream* control-string format-arguments)))

(defun rformat (control-string &rest format-arguments)
  (apply #'format *result-stream* control-string format-arguments))

(defmacro with-result-stream ((fname &rest key-value-pairs &key &allow-other-keys) &body body)
  `(with-open-file (*result-stream* ,fname :direction :output ,@key-value-pairs)
     ,@body))

;;; Utils
(defun maybe (pred)
  #'(lambda (arg)
      (or (funcall pred arg)
	  (null arg))))

(defun nullfn (&rest args)
  (declare (ignore args)))

(defun combine-functions (hf1 hf2)
  "Combine 2 functions in OR"
  (cond
    ((and (functionp hf1) (functionp hf2)
	  (not (eq hf1 #'nullfn)) (not (eq hf2 #'nullfn)))
     #'(lambda (&rest args)
	 (or (apply hf1 args)
	     (apply hf2 args))))
    ((and (not (eq hf1 #'nullfn))(functionp hf1))
     hf1)
    ((and (not (eq hf2 #'nullfn)) (functionp hf2))
     hf2)
    (t #'nullfn)))

(defun group-by-2 (list)
  "(1 2 3 4 5 6 7 8) => ((1 2) (3 4) (5 6) (7 8))
(:a 1 :b 2 :c 3 :d 4) => ((:a 1) (:b 2) (:c 3) (:d 4))"
  (cond
    ((null list) nil)
    ((cons (list (car list) (cadr list)) (group-by-2 (cddr list))))))

(defun get-keyword-value (key list)
  "Given a keyword argument list, returns the value for a given key"
  (let ((p (member key list)))
    (and p (cadr p))))

(defun subst-keyword-value (key value list)
  "Given a keyword argument LIST, returns another list
whith the value associated to KEY changed to VALUE"
  (if (get-keyword-value key list)
      (let ((list-by-2 (group-by-2 list)))
	(flatten (mapcar #'(lambda (el)
			     (if (eq key (car el))
				 (list key value)
				 el))
			 list-by-2)))
      (cons key (cons value list))))

;;;; Binary flags
;; key-alist : binary flag alist of the form
;;    ((1 . :flag-1)
;;     (2 . :flag-2)
;;     (4 . :flag-3)
;;     (8 . :flag-4)
;;     ... )
;; flags: flags in integer form
;; key: keyword
;; value: value associated to the keyword
;;
;; Returns: a copy of FLAGS with the value for KEY changed to VALUE
;; ex. ((1 . :flag-1)
;;      (2 . :flag-2))
;;     (setflag 0 :flag-1 t) => 1
;;     (setflag 3 :flag-1 nil) => 2
;;     (setflag 2 :flag-1 t) => 3
(defun setflag (flags key-alist key value)
  (let ((f (rassoc key key-alist)))
    (cond ((not f) flags)
	  (value (logior flags (car f)))
	  (t (logand (lognot (car f)) flags)))))

;; Same as above but operating on multiple flags
(defun setflags (flags key-alist &rest key-value-pairs &key &allow-other-keys)
  (cond ((null key-value-pairs)
	 flags)
	(t (apply #'setflags
		  (setflag flags key-alist (car key-value-pairs) (cadr key-value-pairs))
		  key-alist
		  (cddr key-value-pairs)))))

;; Gets the value associated to KEY from FLAGS (with respect to KEY-ALIST)
(defun getflag (key flags key-alist)
  (let ((f (rassoc key key-alist)))
    (when f
      (not (zerop (logand (car f) flags))))))

(defun xor (x y)
  (and (or x y) (not (and x y))))

(defun move-next-to-helper (el next-to seq &key (where :right) (test #'eql))
  (if (find el seq :test test)
      (let* ((seq1 (remove el seq :test test))
	     (next-to-pos (position next-to seq1 :test test))
	     (splitpos (and next-to-pos (case where
					  (:right (1+ next-to-pos))
					  (:left next-to-pos))))
	     (subseq1 (and splitpos (subseq seq1 0 splitpos)))
	     (subseq2 (if splitpos (subseq  seq1 splitpos) seq1)))
	(concatenate (type-of seq) subseq1 (make-sequence (type-of seq) 1 :initial-element el) subseq2))
      seq))

(defun move-next-to (el next-to seq &key (where :right) (test #'eql))
  (cond ((not (find el seq :test test))
	 seq)
	((null next-to)
	 (case where
	   (:left (append (remove el seq :test test) (list el)))
	   (:right (cons el (remove el seq :test test)))))
	(t (move-next-to-helper el next-to seq :where where :test test))))

;;; XLib Utils
(defparameter +last-event+ (array-dimension xlib::*event-key-vector* 0))
(defvar handlers (make-array +last-event+))

(defun window-resize (window width height)
  (declare (type window window)
	   (type card32 width height))
  (with-state (window)
    (setf (drawable-width window) width)
    (setf (drawable-height window) height))
  (display-finish-output (window-display window)))

(defun handler-pos (event-key)
  (position event-key xlib::*event-key-vector*))

(defmacro handler-vector (&rest clauses)
  (let ((res)
	(handlers (gensym)))
    (dolist (clause clauses)
      (destructuring-bind (event-match event-slots &rest handler-body)
	  clause
	(let ((eml (if (listp event-match) event-match (list event-match))))
	  (dolist (em eml)
	    (push `(setf (svref ,handlers (handler-pos ,em))
			 #'(lambda (&key ,@event-slots &allow-other-keys)
			     ,@handler-body))
		  res)))))
    `(let ((,handlers (make-array +last-event+ :initial-element #'nullfn)))
       ,@(reverse res)
       ,handlers)))

(defun combine-handlers (h1 h2)
  (assert (and (or (functionp h1) (sequencep h1))
  	       (or (functionp h2) (sequencep h2))))
  (cond
    ((and (sequencep h1) (sequencep h2))
     (map 'vector #'combine-functions
	  h1 h2))
    ((and (functionp h1) (sequencep h2))
     (map 'vector #'(lambda (hf2)
		      (combine-functions h1 hf2))
	  h2))
    ((and (sequencep h1) (functionp h2))
     (map 'vector #'(lambda (hf1)
		      (combine-functions hf1 h2))
	  h1))
    ((and (functionp h1) (functionp h2))
     (combine-functions h1 h2))))

(defun window-parent (win)
  (multiple-value-bind (children parent root)
      (query-tree win)
    (declare (ignore children root))
    parent))

(defun is-toplevel (win)
  (window-equal (window-parent win) (drawable-root win)))

(defun encode-flags (flags flags-alist)
  (reduce #'+ (mapcar #'(lambda (el)
			  (cdr (assoc el flags-alist)))
		      flags)))

(defun decode-flags (flags flags-alist)
  (if (numberp flags)
      (mapcar #'car (remove-if #'(lambda (el)
				   (zerop (logand flags (cdr el))))
			       flags-alist))
      :error))

;;; Core Protocol

;; Protocol version
(defparameter +XEMBED-VERSION+ 0)

;; Internal return codes
(defparameter +XEMBED_RESULT_OK+ 0)
(defparameter +XEMBED_RESULT_UNSUPPORTED+ 1)
(defparameter +XEMBED_RESULT_X11ERROR+ 2)

;; XEMBED messages
(defparameter +XEMBED-MESSAGE-ALIST+
  '((:XEMBED-EMBEDDED-NOTIFY . 0) 	; e -> c :: client embedded
    (:XEMBED-WINDOW-ACTIVATE . 1) 	; e -> c :: embedder gets (keyboard) focus
    (:XEMBED-WINDOW-DEACTIVATE . 2) 	; e -> c :: client loses focus
    (:XEMBED-REQUEST-FOCUS . 3) 	; c -> e :: client requests focus
    (:XEMBED-FOCUS-IN . 4) 		; e -> c :: client gets focus + focus move
    (:XEMBED-FOCUS-OUT . 5)		; e -> c :: client loses focus
    (:XEMBED-FOCUS-NEXT . 6)		; c -> e :: client reached end of the tab focus chain
    (:XEMBED-FOCUS-PREV . 7)		; c -> e :: client reached beg of TFC after a back tab
    (:XEMBED-MODALITY-ON . 10)		; e -> c :: embedder gets shadowed by a modal dialog
    (:XEMBED-MODALITY-OFF . 11)		; e -> c :: 
    (:XEMBED-REGISTER-ACCELERATOR . 12)
    (:XEMBED-UNREGISTER-ACCELERATOR . 13)
    (:XEMBED-ACTIVATE-ACCELERATOR . 14)
    (:XEMBED-GTK-GRAB-KEY . 108)
    (:XEMBED-GTK-UNGRAB-KEY . 109)
    (:XEMBED-PROTOCOL-FINISHED . 201)))  ;; Sent from the socket to its parent to signal the end of the protocol

;; XEMBED-PROTOCOL-FINISHED . 201
;; sent from the socket to its parent when the client finished the protocol for some reason
;; data1 : socket window-id 
;; data2 : client window-id 

;; Details for  XEMBED_FOCUS_IN
(defparameter +XEMBED-DETAIL-ALIST+
  '((:XEMBED-FOCUS-CURRENT . 0)
    (:XEMBED-FOCUS-FIRST . 1)
    (:XEMBED-FOCUS-LAST . 2)))

;; TODO Check errors (nil)
(defun encode-xembed-message-type (type)
  (cdr (assoc type +XEMBED-MESSAGE-ALIST+)))
(defun decode-xembed-message-type (type)
  (car (rassoc type +XEMBED-MESSAGE-ALIST+)))

(defun encode-xembed-detail (detail)
  (cdr (assoc detail +XEMBED-DETAIL-ALIST+)))
(defun decode-xembed-detail (detail)
  (car (rassoc detail +XEMBED-DETAIL-ALIST+)))


;; Modifiers field for XEMBED_REGISTER_ACCELERATOR */
(defparameter +XEMBED-MODIFIER-ALIST+
  `((:XEMBED_MODIFIER_SHIFT . ,(ash 1 0))
    (:XEMBED_MODIFIER_CONTROL . ,(ash 1 1))
    (:XEMBED_MODIFIER_ALT . ,(ash 1 2))
    (:XEMBED_MODIFIER_SUPER . ,(ash 1 3))
    (:XEMBED_MODIFIER_HYPER . ,(ash 1 4))))

(defun encode-xembed-modifier-flags (flags)
  (encode-flags flags +XEMBED-MODIFIER-ALIST+))

(defun decode-xembed-modifier-flags (flags)
  (decode-flags flags +XEMBED-MODIFIER-ALIST+))

;; Flags for XEMBED_ACTIVATE_ACCELERATOR 
(defparameter +XEMBED_ACCELERATOR_OVERLOADED+ (ash 1 0))

;; Directions for focusing
(defparameter +XEMBED_DIRECTION_DEFAULT+ 0)
(defparameter +XEMBED_DIRECTION_UP_DOWN+ 1)
(defparameter +XEMBED_DIRECTION_LEFT_RIGHT+ 2)

;; Flags for _XEMBED_INFO
(defparameter +XEMBED-INFO-FLAGS-ALIST+
  `((:XEMBED-MAPPED . ,(ash 1 0))))

(defun encode-xembed-info-flags (flags)
  (encode-flags flags +XEMBED-INFO-FLAGS-ALIST+))

(defun decode-xembed-info-flags (flags)
  (decode-flags flags +XEMBED-INFO-FLAGS-ALIST+))

(defparameter +CurrentTime+ nil)

;; Last time received
(defparameter *timestamp* 0)

(defun set-property-notify (window)
  (setf (window-event-mask window)
	(logior (window-event-mask window)
		(make-event-mask :property-change))))

(let ((x 0))
  (flet ((some-value () (if (= x 1) (setf x 2) (setf x 1))))
    (defun get-server-time (win)
      (let ((dpy (window-display win)))
	(display-finish-output dpy)
	(change-property win :clx-xembed-timestamp `(,(some-value)) :clx-xembed-timestamp 32)
	(event-cond (dpy :force-output-p t)
	  (:property-notify
	   (window atom time)
	   (and (window-equal window win)
		(eq :clx-xembed-timestamp atom))
	   time))))))

(defun update-timestamp (win &optional timestamp)
  (format t "TIMESTAMP: ~a > ~a = ~a ~%" timestamp *timestamp* (when (and *timestamp* timestamp) (> timestamp *timestamp*)))
  (when (or (zerop *timestamp*) (and (numberp timestamp) (> timestamp *timestamp*)))
    (setf *timestamp* (or timestamp (get-server-time win))))
  *timestamp*)

(defun xembed-info-raw (window)
  (get-property window :_XEMBED_INFO))

;; Protocol version
(defun xembed-info-version-raw (xembed-info) 
  (first xembed-info))

;; Flags: Only one flag is supported: XEMBED_MAPPED
(defun xembed-info-flags-raw (xembed-info) ;; flags 
  (second xembed-info))

(defun xembed-info (window)
  (let ((prop (get-property window :_XEMBED_INFO)))
    (list (xembed-info-version prop)
	  (decode-xembed-info-flags (xembed-info-flags-raw prop)))))

(defun xembed-info-valid-p (xembed-info)
  (and (numberp (xembed-info-version xembed-info))
       (listp (xembed-info-flags xembed-info))))

(defun xembed-info-ready-p (window)
  (let ((info (xembed-info window)))
    (and info (xembed-info-valid-p info))))

;; Protocol version
(defun xembed-info-version (xembed-info) 
  (first xembed-info))

(defun xembed-info-flags (xembed-info)
  (second xembed-info))

;;; Setters
(defun (setf xembed-info-raw) (new-val window)
  (change-property window :_XEMBED_INFO new-val :_XEMBED_INFO 32))

(defun (setf xembed-info) (new-val window)
  (destructuring-bind (version flags) new-val
    (setf (xembed-info-raw window)
	  (list version (encode-xembed-info-flags flags))))
  new-val)

;; FIXME: setters for properties should check if the property exist
(defun (setf xembed-info-version) (new-val window)
  (let ((info (xembed-info window)))
    (assert (not (null info)))
    (setf (xembed-info window) (list new-val (xembed-info-flags-raw info)))))

(defun (setf xembed-info-flags) (new-val window)
  (let ((info (xembed-info window)))
    (assert (not (null info)))
    (setf (xembed-info window) (list (xembed-info-version info)
				     new-val))))

;;;; XEmbed message sending 
;; Wrap in handler-case, sync with `display-finish-output' and
;; handle errors
;; FIXME: change current time to something more meaningful
(defun xembed-send (dest-win &key opcode (detail 0) (data1 0) (data2 0) timestamp)
  (assert (not (null *timestamp*)))
  (dformat 7 ">>> xembed-send~S~%" (list (list (window-id dest-win))
					 (or (and (keywordp opcode) opcode) (decode-xembed-message-type opcode))
					 detail data1 data2))
  (send-event dest-win :client-message nil
		       :type :_XEMBED
		       :format 32
		       :window dest-win
		       :data (list (or timestamp *timestamp*)
			           (or (and (numberp opcode) opcode) (encode-xembed-message-type opcode))
			           (or (and (numberp detail) detail) (encode-xembed-detail detail))
			           data1 data2)
		       :propagate-p nil)
  (dformat 7 "<<< xembed-send~%"))

(defun xembed-notify (client-window embedder-window &optional version)
  (dformat 7 ">>> xembed-notify~%")
  (xembed-send client-window
	       :opcode :xembed-embedded-notify
	       :data1 (window-id embedder-window)
	       :data2 (or version +XEMBED-VERSION+))
  (dformat 7 "<<< xembed-notify~%"))

(defun xembed-focus-in (client-window detail)
  (dformat 7 ">>> xembed-focus-in~%")
  (xembed-send client-window
	       :opcode :xembed-focus-in
	       :detail detail)
  (dformat 7 "<<< xembed-focus-in~%"))

(defun delete-create-notify-event (win)
  (display-force-output (window-display win))
  (event-cond ((window-display win))
    (:create-notify (window) (window-equal window win)
		    t)))

(defmacro send-wrapper (disp (&body send-sequence)
			&body error-handlers)
  `(handler-case
       (progn
	 ,@send-sequence
	 (display-finish-output ,disp))
     ,@error-handlers))

(defun supported-protocol-version (win)
  (let* ((info (xembed-info win))
	 (vers (and info (xembed-info-version info))))
    (format t "INFO: ~a~%" info)
    (and (listp (xembed-info-flags info)) (numberp vers) (min +XEMBED-VERSION+ vers))))

;;; Socket
;; Property :XEMBED-SOCKET-INFO
;; (use-client-geometry protocol-started-p modality active focused)
(defparameter +xembed-socket-info-flags-alist+
  '((1 . :use-client-geometry-p)
    (2 . :protocol-started-p)
    (4 . :modality-on-p)
    (8 . :active-p)
    (16 . :focused-p)
    (32 . :focus-requested-p)))

(defun xsi-flags (xsi)
  (first xsi))

(defun xsi-setflags (xsi &rest keyword-value-pairs &key &allow-other-keys)
  (list (apply #'setflags (xsi-flags xsi) +xembed-socket-info-flags-alist+ keyword-value-pairs)))

(defun xsi-getflag (xsi key)
  (getflag key (xsi-flags xsi) +xembed-socket-info-flags-alist+))

(defun xembed-socket-info (window)
  (let ((prop (get-property window :XEMBED-SOCKET-INFO)))
    (when prop
      (list (first prop)))))

(defun (setf xembed-socket-info) (value window)
  (change-property window :XEMBED-SOCKET-INFO (list (xsi-flags value))
		          :XEMBED-SOCKET-INFO 32))

(defparameter +socket-event-mask+
  (make-event-mask  :substructure-notify :exposure :enter-window
		                         :leave-window :button-press 
		                         :property-change))

(defun create-socket (use-client-geometry &rest key-value-pairs &key &allow-other-keys)
  (let* ((em (get-keyword-value :event-mask key-value-pairs))
	 (em1 (logior +socket-event-mask+ (or em 0)))
	 (sockwin (apply #'create-window (subst-keyword-value :event-mask em1 key-value-pairs))))
    (let ((xsi (list 0)))
      (setf (xembed-socket-info sockwin) (xsi-setflags xsi :use-client-geometry-p use-client-geometry)))
    sockwin))

(defun socketp (window)
  (xembed-socket-info window))

(defun client (win)
  (multiple-value-bind (children parent root)
      (query-tree win)
    (declare (ignore parent root))
    (when children 
      (car children))))

(defun requested-state (win)
  (let ((c (client win)))
    (let ((info (xembed-info c)))
      (cond ((null info)
	     :error)
	    ((eq :error (xembed-info-flags info))
	     :error)
	    ((member :xembed-mapped
		     (xembed-info-flags info))
	     :mapped)
	    (t :unmapped)))))

(defun forward-xembed-message-up (socketwin timestamp opcode detail data1 data2)
  (xembed-send (window-parent socketwin)
	       :opcode opcode
	       :detail detail
	       :timestamp timestamp
	       :data1 data1
	       :data2 data2))

(defun maybe-start-protocol (socketwin)
  (let* ((xsi (xembed-socket-info socketwin))
	 (client (client socketwin))
	 (ver (supported-protocol-version client))
	 (dpy (window-display socketwin)))
    ;;(display-finish-output dpy)
    (when client
      (cond
	((xsi-getflag xsi :protocol-started-p)
	 t)
	(ver
	 (send-wrapper dpy
	     ((xembed-notify client
			     socketwin
			     ver)
	       (xembed-focus-in client :xembed-focus-current)
	       (xembed-send client :opcode :xembed-window-activate)
	       (xembed-send client :opcode :xembed-modality-on)
	       (cond ((not (xsi-getflag xsi :focused-p))
		      (xembed-send client :opcode :xembed-focus-out))
		     (t (xembed-focus-in client :xembed-focus-first)))
	       (when (not (xsi-getflag xsi :active-p))
		 (xembed-send client :opcode :xembed-window-deactivate))
	       (when (not (xsi-getflag xsi :modality-on-p))
		 (xembed-send client :opcode :xembed-modality-off))
	       (setf (xembed-socket-info socketwin)
		     (xsi-setflags xsi :protocol-started-p t))
	       (display-finish-output dpy)
	       t)
	   (xlib::x-error () (format t "ERROR on start protocol ~%") nil)))
	(t nil)))))

(defmacro send-and-set-flag (socketwin flag-key flag-value &body send-form)
  (let ((xsi (gensym))
	(sw (gensym))
	(fv (gensym))
	(fk (gensym)))
    `(let* ((,sw ,socketwin)
	    (,xsi (xembed-socket-info ,sw))
	    (,fv ,flag-value)
	    (,fk ,flag-key))
       (display-finish-output (window-display ,sw))
       (setf (xembed-socket-info ,sw)
	     (xsi-setflags ,xsi ,fk ,fv))
       (format t "XSI~S, [~S, ~S], V ~S, STARTED ~S~%" ,xsi ,fk ,fv (xsi-getflag ,xsi ,fk) (xsi-getflag ,xsi :protocol-started-p))
       (when (and ,xsi (xor (xsi-getflag ,xsi ,fk) ,fv)
		  (xsi-getflag ,xsi :protocol-started-p))
	 (send-wrapper (window-display ,sw)
	     (,@send-form)
	   (xlib::x-error () (error "motita")))))))

(defun socket-focus-in (socketwin &optional (what :xembed-focus-current))
  (socket-clear-focus-request socketwin)
  (send-and-set-flag socketwin :focused-p t
    (xembed-focus-in (client socketwin) what)))

(defun socket-focus-out (socketwin)
  (send-and-set-flag socketwin :focused-p nil
    (xembed-send (client socketwin) :opcode :xembed-focus-out)))

(defun socket-focus-requested-p (socketwin)
  (xsi-getflag (xembed-socket-info socketwin) :focus-requested-p))

(defun socket-clear-focus-request (socketwin &optional value)
  (let ((xsi (xembed-socket-info socketwin)))
    (setf (xembed-socket-info socketwin) (xsi-setflags xsi :focus-requested-p value))))

(defun socket-activate (socketwin)
  (send-and-set-flag socketwin :active-p t 
    (xembed-send (client socketwin) :opcode :xembed-window-activate)))

(defun socket-deactivate (socketwin)
  (send-and-set-flag socketwin :active-p nil 
    (xembed-send (client socketwin) :opcode :xembed-window-deactivate)))

(defun socket-modality-on (socketwin)
  (send-and-set-flag socketwin :modality-on-p t 
    (xembed-send (client socketwin) :opcode :xembed-modality-on)))
(defun socket-modality-off (socketwin)
  (send-and-set-flag socketwin :modality-on-p nil 
    (xembed-send (client socketwin) :opcode :xembed-modality-off)))

(defun destroy-socket (socketwin &optional (reparent-p t))
  (when (and reparent-p (client socketwin))
    (unmap-window (client socketwin))
    (reparent-window (client socketwin)
		     (drawable-root socketwin)
		     0 0))
  (destroy-window socketwin))

(defun socket-reset (socketwin)
  (when (client socketwin)
    (reparent-window (client socketwin)
		     (drawable-root socketwin)
		     0 0))
  (let ((xsi (xembed-socket-info socketwin)))
    (setf (xembed-socket-info socketwin)
	  (xsi-setflags xsi :protocol-started-p nil
			    :modality-on-p nil
			    :active-p nil
			    :focused-p nil))))

(defun satisfy-map/unmap-request (socketwin)
  (case (requested-state socketwin)
    ((:error :mapped) (map-window (client socketwin)))
    (:unmapped (unmap-window (client socketwin)))))

(defun socket-reset-geometry (socketwin)
  (let ((xsi (xembed-socket-info socketwin)))
    (cond ((xsi-getflag xsi :use-client-geometry-p)
	   (let ((sh (wm-normal-hints (client socketwin))))
	     (window-resize socketwin (wm-size-hints-width sh) (wm-size-hints-height sh))))
	  (t (window-resize (client socketwin) (drawable-width socketwin) (drawable-height socketwin))
	     (drop-configure-notify socketwin (drawable-width socketwin) (drawable-height socketwin))))))

(defun embed (socketwin clientwin &optional (reparent-p nil) (x 0) (y 0) (reset-geometry-p t))
  (set-property-notify clientwin)
  (display-finish-output (window-display socketwin)) ;; Throw error if bad window
  (when reparent-p
    (reparent-window clientwin socketwin x y))
  (when reset-geometry-p
    (socket-reset-geometry socketwin))
  (maybe-start-protocol socketwin))

;;;; Basic event processing
(defun socket-list-handler-vector (socketlist-fn)
  (flet ((socketlist-member (window)
	   (member window (funcall socketlist-fn) :test #'window-equal)))
    (handler-vector
     ((:configure-notify) (event-window)
      (when (socketlist-member event-window)
	(format t "CONFIGURE-NOTIFY RECEIVED~%")
	(socket-reset-geometry event-window)))
     ((:destroy-notify) (event-window window)
      (when (socketlist-member event-window)
	(socket-reset event-window)
	(xembed-send (window-parent event-window)
		     :opcode :xembed-protocol-finished
		     :data1 (window-id event-window)
		     :data2 (window-id window))))
     ((:create-notify) (parent window)
      (when (socketlist-member parent)
	(embed parent window nil)))
     ((:reparent-notify) (event-window parent window)
      (when (socketlist-member event-window)
	(cond ((window-equal event-window parent)
	       (embed parent window nil)
	       t)
	      (t (socket-reset event-window)
		 (xembed-send (window-parent event-window)
			      :opcode :xembed-protocol-finished
			      :data1 (window-id event-window)
			      :data2 (window-id window))))))
     ((:property-notify) (window atom time)
      (handler-case 
	  (let ((parent (window-parent window)))
	    (when (and (eq atom :_XEMBED_INFO)
		       (socketlist-member parent))
	      (update-timestamp parent time)
	      (when (maybe-start-protocol parent)
		(satisfy-map/unmap-request parent))
	      t))
	(xlib::x-error () nil))))))

(defun socket-handler-vector (socketwin)
  (socket-list-handler-vector #'(lambda () (list socketwin))))

(defun drop-configure-notify (socketwin w h)
  (declare (type window socketwin)
	   (type card32 w h))
  (event-cond ((window-display socketwin) :timeout 0)
    (:configure-notify (event-window window width height)
		       (and (window-equal event-window socketwin)
			    (window-equal window (client socketwin))
			    (= w (the card32 width)) (= h (the card32 height)))
		       t)))

(defun socket-resize (socketwin w h)
  (declare (type window socketwin)
	   (type card32 w h))
  (when (client socketwin)
    (window-resize (client socketwin) w h)
    (drop-configure-notify socketwin w h))
  (window-resize socketwin w h))
