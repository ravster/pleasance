;;;; This file will load everything I need, in order.  I might one day turn this into a system and use ASDF.  Who knows?

;; Change to the correct directory.
(setf *default-pathname-defaults* #p"/home/ravi/trading/neural-net/")

;; Load cl-ppcre
(ql:quickload "cl-ppcre")

(load "defpackage.lisp")
(load "data-mgmt.lisp")
(load "indicators.lisp")
(load "training-data-creation.lisp")
