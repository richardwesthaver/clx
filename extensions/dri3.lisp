;;; dri3.lisp --- DRI3 Extension

;; 

;;; Code:
(in-package :xlib)

(define-extension "DRI3")

(defun dri3-opcode (display)
  (extension-opcode display "DRI3"))

(defconstant +dri-major+ 1)
(defconstant +dri-minor+ 4)
