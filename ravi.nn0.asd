;;;; This defines the system file for the package ravi.nn0

(defpackage :ravi.nn0-system
  (:use :cl :asdf))

(in-package :ravi.nn0-system)

(defsystem "ravi.nn0"
  :description "GA & NN system for forecasting the direction of a market."
  :version "0.1.0"
  :author "Ravi Desai <rd7190@gmail.com>"
  :components ((:file "defpackage")
	       (:file "data-mgmt" :depends-on ("defpackage"))
	       (:file "indicators" :depends-on ("data-mgmt"))
	       (:file "training-data-creation" :depends-on ("indicators"))
	       (:file "nn-for-ga" :depends-on ("training-data-creation"))
	       (:file "ga" :depends-on ("nn-for-ga")))
  :depends-on (:cl-ppcre))