(defpackage #:xlib/tests
  (:use :cl :rt)
  (:export #:run-all-tests #:xlib-test #:xlib-all-tests))
(defpackage #:clx/truetype/tests
  (:nicknames :xft-test)
  (:use #:cl #:xft #:ttf)
  (:export show-window))
(in-package #:xlib/tests)
(defsuite :xlib)
