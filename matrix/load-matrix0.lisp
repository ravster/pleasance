;;;; This file will load everything I need, in order.  I might one day turn this into a system and use ASDF.  Who knows?

;; Change to the correct directory.
(setf *default-pathname-defaults* #p"/home/ravi/trading/neural-net/matrix/")

;; Load cl-ppcre
(ql:quickload "cl-ppcre")

;; Load gsll
(ql:quickload "gsll")

(load "../defpackage.lisp")		;Define ravi.nn0 package
(load "../data-mgmt.lisp")		;Define all the functions in RAVI.NN0 that we want to use
(load "../indicators.lisp")		;Same
(load "../training-data-creation.lisp")	;Same

(load "defpackage-matrix0.lisp")	;Define this (MATRIX0) package
