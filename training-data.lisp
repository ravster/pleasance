;;;; This code creates the training data set that will be used by the neural-net as input.

;;; Lets set up the data structure first.
(defparameter training-set (make-array '(5000 3))
  "This is the training set data.")

;; Normalize data
; from -1 to 1: "score/(range/2)" where range = abs(min) + abs(max)
; from 0 to 1: "score-min / range"

;; Todo: normalize ma-diff-close
; figure out output measure
; todo: normalize output
; place normalized values into training data

(defun score-madiffclose (raw-data output-array)
  "Normalize the data of MA - closing price and place it in the first column of the training-set array."
  (do ((denominator (/ (+ (abs (loop for i from 20 below 5020
				  maximize (ma-diff-close (aref raw-data i))))
			  (abs (loop for i from 20 below 5020
				  minimize (ma-diff-close (aref raw-data i)))))
		       2))
       (i 0 (incf i)))
      ((= i 5000))
    (setf (aref output-array i 0)
	  (/ (ma-diff-close (aref raw-data (+ i 20)))
	     denominator))))

(score-madiffclose *array* training-set)