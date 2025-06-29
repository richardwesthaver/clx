(defpackage #:xlib/tests
  (:shadowing-import-from :stumpwm :version :message)
  (:use :cl :rt :stumpwm)
  (:export #:run-all-tests #:xlib-test #:xlib-all-tests))
(in-package #:xlib/tests)
(defsuite :xlib)
