;; This class holds all the variables that we shall be working with, for each bar of the data.
(defclass bar ()
  ((openb :initarg :open :reader openb :type single-float)
   (high :initarg :high :reader high :type single-float)
   (low :initarg :low :reader low :type single-float)
   (closeb :initarg :close :reader closeb :type single-float)
   (sqnb :initform 0 :accessor sqnb)	;SQN of last N bars.
   (atrb :initform 0 :accessor atrb)	;ATR of last N bars.
   (ma-20 :initform 0 :accessor ma-20)	;MA-20 of the close.
   (ma-diff-close :accessor ma-diff-close) ;MA minus close (For NN input).
   (trb :initform 0 :accessor trb))	;TR of this bar.
  (:documentation "This object defines the price-points and other qualities of a single bar."))

;; The data in this vector is ordered from oldest to newest.
(defparameter *array* (make-array 60000 :fill-pointer 0 :adjustable t :element-type 'bar))

;; This function reads in the csv-file and places objects of the 'bar' class into *array*
(defun read-ohlc (file-name array-name)
  "This function reads from 'file-name' and puts bar-objects in 'array-name'."
  (let ((temp nil))
    (with-open-file (file file-name)
	(loop for line = (read-line file nil) ;The 'nil' is so that there is no EOF error.
	   while line do
	     (setf temp (cl-ppcre:split "," line))
	     (vector-push-extend (make-instance 'bar 
						:open (read-from-string (first temp)) 
						:high (read-from-string (third temp))
						:low (read-from-string (second temp)) 
						:close (read-from-string (fourth temp)))
				 array-name)))))

;Data from http://www.fxhistoricaldata.com/

;; Perform actions
(read-ohlc "/home/ravi/trading/GBPUSD_hour.csv" *array*)	;Random - median = $10057 won.  0.04% lose.  Amazing.
