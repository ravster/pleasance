;; Find the true range for each bar and place into object.
(defun calculate-true-range (data)
  "This function will traverse a vector of BAR objects and calculate the true range for each."
  (loop for i from 1 below (length data)
     do
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
  (loop for i from n below (length data)
     do
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
k	    (format stream "~&~{~A,~}" (reverse row)))))))

;; Calculate moving average.
(defun moving-average (data n)
  "This function calculates the moving average of 'ma-source' values for 'n' periods in array 'data & puts the result in 'ma-period' of the bar-element in the 'data' array."
  (loop for i from n below (length data)
     do
     (setf (ma-20 (aref data i))
	   (/ (do ((x (- i n) (incf x))
		   (sum 0))
		  ((= x i) sum)
		(incf sum (closeb (aref data x))))
	      n))))

(moving-average *array* 20)

;; Calculate moving average difference from close
(defun ma-difference-from-close (data n)
  "This will be the signal that we want for input to the NN."
  (loop for i from n below (length data)
     do
     (setf (ma-diff-close (aref data i))
	   (- (ma-20 (aref data i))
	      (closeb (aref data i))))))

(ma-difference-from-close *array* 20)

;; Calculate Close+5 minus Close
(defun +5close-present (raw-data)
  "Find the difference between the close 5 periods into the future and the close right now."
  (loop for i from 0 below (- (length raw-data)
			      5)
     do
     (setf (+5close-diff (aref raw-data i))
	   (- (closeb (aref raw-data (+ i 5)))
	      (closeb (aref raw-data i))))))

(+5close-present *array*)