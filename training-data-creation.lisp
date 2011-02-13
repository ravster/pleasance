;;;; This code creates the training data set that will be used by the neural-net as input.

(in-package :ravi.nn0)

;;; Lets set up the data structure first.
(defparameter training-set (make-array '(5000 3))
  "This is the training set data.")

;; Normalize data
;; Min-Max normalization
;; Score = (max_range - min_range) * ((Data - min_data)/(Max_data - min_data)) + min_range

(defun score-madiffclose (raw-data output-array)
  "Normalize the data of (MA - closing price) and place it in the first column of the training-set array."
  (loop for i from 0 below 5000
     with max-range = 1
     with min-range = -1
     with distance-range = (- max-range min-range)
     with max-data = (loop for i from 20 below 5020
			maximize (ma-diff-close (aref raw-data i)))
     with min-data = (loop for i from 20 below 5020
			minimize (ma-diff-close (aref raw-data i)))
     with distance-data = (- max-data min-data)
     do
     (setf (aref output-array i 0)
	   (+ min-range
	      (* distance-range
		 (/ (- (ma-diff-close (aref raw-data (+ i 20)))
		       min-data)
		    distance-data))))))		       

(score-madiffclose *array* training-set)

(defun score-+5closediff (raw-data output-array)
  "Normalize '+5close-diff' (Close of 5 periods in the future and the present close)in the bar-array and place the normalized values in the out-put array."
  (loop for i from 0 below 5000
     with max-range = 1
     with min-range = -1
     with distance-range = (- max-range min-range)
     with max-data = (loop for i from 20 below 5020
			maximize (+5close-diff (aref raw-data i)))
     with min-data = (loop for i from 20 below 5020
			minimize (+5close-diff (aref raw-data i)))
     with distance-data = (- max-data min-data)
     do
     (setf (aref output-array i 2)
	   (+ min-range
	      (* distance-range
		 (/ (- (+5close-diff (aref raw-data (+ i 20)))
		       min-data)
		    distance-data))))))

(score-+5closediff *array* training-set)

(defun score-atrb (raw-data output-array)
  "Normalized score of ATR."
  (loop for i from 0 below 5000
     with max-range = 1
     with min-range = -1
     with distance-range = (- max-range min-range)
     with max-data = (loop for i from 20 below 5020
			maximize (atrb (aref raw-data i)))
     with min-data = (loop for i from 20 below 5020
			minimize (atrb (aref raw-data i)))
     with distance-data = (- max-data min-data)
     do
     (setf (aref output-array i 1)
	   (+ min-range
	      (* distance-range
		 (/ (- (atrb (aref raw-data (+ i 20)))
		       min-data)
		    distance-data))))))

(score-atrb *array* training-set)

;;;; Here we shall create the validation set data.

(defparameter validation-set (make-array 5000)
  "This dataset will be the validation set, to make sure we are not overtraining the data.")

(defun validation-set-data-creation (raw-data output-array)
  "Normalize '+5close-diff' (Close of 5 periods in the future and the present close)in the bar-array and place the normalized values in the out-put array."
  (loop for i from 0 below 5000
     with denominator = (/ (+ (abs (loop for i from 5020 below 10020
				      maximize (+5close-diff (aref raw-data i))))
			      (abs (loop for i from 5020 below 10020
				      minimize (+5close-diff (aref raw-data i)))))
			   2)
        ;Denominator gives us the "range" of values of the raw numbers we are trying to normalize.  Range is now between -1 & 1.
     do
     (setf (aref output-array i)
	   (/ (+5close-diff (aref raw-data (+ i 5020)))
	      denominator))))

(validation-set-data-creation *array* validation-set)
