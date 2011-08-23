;; Find the true range for each bar and place into object.

(in-package :ravi.nn0)

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
	    (format stream "~&~{~A,~}" (reverse row)))))))

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

;; Calculate + & - DMs.
(defun calc-dm (data)
  ""
  (loop for i from 1 below (length data)
     do
     (let ((up (- (high (aref data i))		;present high
		  (high (aref data (1- i)))))	;previous high
	   (down (- (low (aref data (1- i)))	;previous low
		    (low (aref data i)))))	;present low
       (if (> up down)
	   (if (plusp up)
	       (setf (+dm (aref data i))
		     up))
	   (if (plusp down)
	       (setf (-dm (aref data i))
		     down))))))

(calc-dm *array*)

;; Calculate + & - DIs.
(defun calc-di (data)
  ""
  (loop for i from 1 below (length data)
     do
     (unless (= 0 (trb (aref data i)))
       (setf (+di (aref data i))
	     (/ (+dm (aref data i))
		(trb (aref data i))))
       (setf (-di (aref data i))
	     (/ (-dm (aref data i))
		(trb (aref data i)))))))

(calc-di *array*)

;; Calculate average DIs.
(defun calc-avg-di (data n)
  ""
  (loop for i from n below (length data)
     do
     (setf (+diavg (aref data i))
	   (/ (loop for j from (- i (1- n)) upto i ;Last n bars (Including i)
		 sum (+di (aref data j)))
	      n))
     (setf (-diavg (aref data i))
	   (/ (loop for j from (- i (1- n)) upto i ;Last n bars (Including i)
		 sum (-di (aref data j)))
	      n))))

(calc-avg-di *array* 14)

;; Calculate DMIs.
(defun calc-dmi (data n)
  ""
  (loop for i from n below (length data)
     do
     (let ((bar (aref data i)))
       (setf (dmi bar)
	     (/ (abs (- (+diavg bar)
			(-diavg bar)))
		(+ (+diavg bar)
		   (-diavg bar)))))))

(calc-dmi *array* 14)

;; Calculate ADX.
(defun calc-adx (data n)
  ""
  (loop for i from (+ n (1- n))	;Since ADX is moving average of DMI which is moving average of DIs.
     below (length data)
     do
     (setf (adx (aref data i))
	   (/ (loop for j from (- i (1- n)) upto i
		 sum (dmi (aref data j)))
	      n))))

(calc-adx *array* 14)

(defun stochastic-oscillator (dataset n)
  (flet ((lowest-low (i)
	   (loop for j from (- i n) upto i
	      minimize (low (aref dataset j))))
	 (highest-high (i)
	   (loop for j from (- i n) upto i
	      maximize (high (aref dataset j)))))
    (loop for i from n below (length dataset) do
	 (setf (so (aref dataset i))
	       (/ (- (closeb (aref dataset i))
		     (lowest-low i))
		  (- (highest-high i)
		     (lowest-low i)))))))

(stochastic-oscillator *array* 20)

(defun moving-stochastic-oscillator (dataset n)
  "Moving average of the stochastic oscillator over 'n' periods."
  (loop for i from (* 2 n) below (length dataset) do ;We are assuming the stochastic oscillator is for the same number of periods, and so a simple doubling works.
       (setf (mso (aref dataset i))
	     (/ (loop for j from (1+ (- i n)) upto i
		     sum (so (aref dataset j)))
		n))))

(moving-stochastic-oscillator *array* 20)

(defun slow-stochastic-oscillator (dataset n)
  "Moving average of MSO."
  (loop for i from (* 3 n) below (length dataset) do
       (setf (sso (aref dataset i))
	     (/ (loop for j from (1+ (- i n)) upto i
		     sum (mso (aref dataset i)))
		n))))

(slow-stochastic-oscillator *array* 20)

(defun rate-of-change (dataset n)
  (loop for i from n below (length dataset) do
       (setf (roc (aref dataset i))
	     (/ (closeb (aref dataset i))
		(closeb (aref dataset (- i n)))))))

(rate-of-change *array* 10)

(defun calc-momentum (dataset n)
  (loop for i from n below (length dataset) do
       (setf (momentum (aref dataset i))
	     (- (closeb (aref dataset i))
		(closeb (aref dataset (- i n)))))))

(calc-momentum *array* 10)

(defun moving-variance (dataset n)
  (loop for i from n below (length dataset) do
       (setf (movar (aref dataset i))
	     (/ (expt (- (closeb (aref dataset i))
			 (/ (loop for j from (- i n) upto i
			       sum (closeb (aref dataset j)))
			    n))
		      2)
		n))))

(moving-variance *array* 20)

(defun ma (length-of-average end-point dataset function)
  "Give the average of the value returned by the function 'function' for 'length-of-average' datapoint ending with the datapoint 'end-point'."
  (/ (loop for i from (- end-point (1- length-of-average)) upto end-point
       sum (funcall function (aref dataset i)))
     length-of-average))

(defun disparity-5-calc (dataset)
  "Return close/average-of-last-5-closes."
  (loop for i from 5 below (length dataset) do
       (setf (disparity-5 (aref dataset i))
	     (/ (closeb (aref dataset i))
		(ma 5 i dataset #'closeb)))))

(disparity-5-calc *array*)

(defun disparity-10-calc (dataset)
  "Return close/average-of-last-10-closes."
  (loop for i from 10 below (length dataset) do
       (setf (disparity-10 (aref dataset i))
	     (/ (closeb (aref dataset i))
		(ma 10 i dataset #'closeb)))))

(disparity-10-calc *array*)

(defun price-oscillator-calc (dataset)
  "(MA_5 - MA_10) / MA_5"
  (loop for i from 10 below (length dataset) do
       (setf (price-oscillator (aref dataset i))
	     (/ (- (ma 5 i dataset #'closeb)
		   (ma 10 i dataset #'closeb))
		(ma 5 i dataset #'closeb)))))

(price-oscillator-calc *array*)
