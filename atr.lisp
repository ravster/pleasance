;; Find the true range for each bar and place into object.
(defun calculate-true-range (data)
  "This function will traverse a vector of BAR objects and calculate the true range for each."
  (do ((i 1 (1+ i))
       (length-of-array (length data)))
      ((>= i length-of-array))
    (let ((high (high (aref data i)))
	  (low (low (aref data i)))
	  (previous-close (closeb (aref data (1- i)))))
      (setf (trb (aref data i))
	    (max (- high low)
		 (abs (- high previous-close))
		 (abs (- previous-close low)))))))

;; Calculate the ATR of last N bars.
(defun calculate-atr (data n)
  "This function should calculate the ATR of the last N bars in array 'data'."
  (do ((i n (1+ i))
       (length-of-array (length data)))
      ((>= i length-of-array))
    (setf (atrb (aref data i))
	  (/ (do ((x (- i n) (incf x))
		  (sum 0))
		 ((> x i) sum)
	       (incf sum (trb (aref data x))))
	     n))))

;; Perform actions
(calculate-true-range *array*)
(calculate-atr *array* 20)

;; This function prints a 1- or 2-dimensional array onto a CSV-file.
(defun array-to-csv (array csv-file)
  "Print a <3-dimension array to a CSV-file."
  (with-open-file (stream csv-file :if-exists :overwrite :direction :output :if-does-not-exist :create)
    (if (eql (second (array-dimensions array))
	     nil)
	(dotimes (row-iterator (array-dimension array 0)) ;If only one dimension.
	  (format stream "~&~A" (aref array row-iterator)))
	(dotimes (row-iterator (array-dimension array 0)) ;If more than one dimension.
	  (let ((row ()))
	    (dotimes (column-iterator (array-dimension array 1))
	      (push (aref array row-iterator column-iterator) row))
	    (format stream "~&~{~A,~}" (reverse row)))))))

;; This is the average ATR
(defparameter *average-atr* nil
  "This is the average ATR over a set of data (Usually the training data).")

(defparameter *vector-of-data* (make-array 50000 :fill-pointer 0 :adjustable t :element-type 'float))

(defun vector-of-data (data n accessor-function)
  "Copy the data values from *array* onto a new vector, for placing into a csv file for graphical viewing of distribution.  'Accessor-function is the accessor function for the data value you wish to evaluate."
  (do ((i n (1+ i))
       (length-of-array (length data)))
      ((>= i length-of-array))
    (vector-push-extend (funcall accessor-function (aref data i))
			*vector-of-data*)))

(vector-of-data *array* 20 #'atrb)

(defun move-decimal-point (array-name shift-by)
  "Move the decimal point 'shift-by' places to the right for each value in the array 'array-name'."
  (do* ((length-of-array (length array-name))
	(i 0 (1+ i)))
       ((> i length-of-array))
    (setf (aref array-name i)
	  (* (aref array-name i)
	     (expt 10 shift-by)))))

;(move-decimal-point *vector-of-data* 4)

;(array-to-csv *vector-of-data* "vectorofdata.csv")

