;; This class holds all the variables that we shall be working with, for each bar of the data.

(in-package :ravi.nn0)

(defclass bar ()
  ((openb :initarg :open :reader openb :type single-float)
   (high :initarg :high :reader high :type single-float)
   (low :initarg :low :reader low :type single-float)
   (closeb :initarg :close :reader closeb :type single-float)
   (atrb :initform 0 :accessor atrb)	;ATR of last N bars.
   (ma-20 :initform 0 :accessor ma-20)	;MA-20 of the close.
   (ma-diff-close :accessor ma-diff-close) ;MA minus close (For NN input).
   (trb :initform 0 :accessor trb)	;TR of this bar.
   (+dm :initform 0 :accessor +dm)
   (-dm :initform 0 :accessor -dm)
   (+di :initform 0 :accessor +di)
   (-di :initform 0 :accessor -di)
   (+diavg :initform 0 :accessor +diavg)
   (-diavg :initform 0 :accessor -diavg)
   (dmi :initform 0 :accessor dmi)
   (adx :initform 0 :accessor adx)
   (+5close-diff :initform 0 :accessor +5close-diff)) ;Difference between the close of 5 periods in the future and right now.
  (:documentation "This object defines the price-points and other qualities of a single bar."))

;; The data in this vector is ordered from oldest to newest.
(defparameter *array* (make-array 4000 :fill-pointer 0 :adjustable t :element-type 'bar))

;; This function reads in the csv-file and places objects of the 'bar' class into *array*
(defun read-ohlc (file-name array-name)
  "This function reads from 'file-name' and puts bar-objects in 'array-name'."
  (let ((temp nil))
    (with-open-file (file file-name)
	(loop for line = (read-line file nil) ;The 'nil' is so that there is no EOF error.
	     repeat (first (array-dimensions array-name)) do ;Fill up the bar-array.
	     (setf temp (cl-ppcre:split "," line))
	     (vector-push-extend (make-instance 'bar 
						:open (read-from-string (fourth temp)) 
						:high (read-from-string (sixth temp))
						:low (read-from-string (fifth temp)) 
						:close (read-from-string (seventh temp)))
				 array-name)))))

;Data from http://www.fxhistoricaldata.com/

;; Populate the bar-array.
(read-ohlc "/home/ravi/trading/GBPUSD_hour.csv" *array*)
