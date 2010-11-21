;;;; These functions extract some data from the bar-array into a vector, which can then be viewed (As a csv-file) by gnumeric.

;; This is the average ATR
;(defparameter *average-atr* nil
;  "This is the average ATR over a set of data (Usually the training data).")

(defparameter *vector-of-data* (make-array 50000 :fill-pointer 0 :adjustable t :element-type 'float))

(defun vector-of-data (data n accessor-function)
  "Copy the data values from 'data' onto a new vector, for placing into a csv file for graphical viewing of distribution.  'Accessor-function is the accessor function for the data value you wish to evaluate."
  (do ((i n (1+ i))
       (length-of-array (length data)))
      ((>= i length-of-array))
    (vector-push-extend (funcall accessor-function (aref data i))
			*vector-of-data*)))

(vector-of-data *array* 20 #'ma-20)

(defun move-decimal-point (array-name shift-by)
  "Move the decimal point 'shift-by' places to the right for each value in the array 'array-name'.  This is done so that we can see the 'pips' instead of seeing a bunch of float-numbers.  Its easier on the eyes."
  (do* ((length-of-array (length array-name))
	(i 0 (1+ i)))
       ((> i length-of-array))
    (setf (aref array-name i)
	  (* (aref array-name i)
	     (expt 10 shift-by)))))

(move-decimal-point *vector-of-data* 4)

(array-to-csv *vector-of-data* "vectorofdata.csv")