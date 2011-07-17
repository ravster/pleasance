(in-package :ravi.nn0)

(defun right-direction? (start-index end-index)
  "Did the NN predict the correct direction of the market for the next 5 periods?"
  (loop for i from start-index below end-index
     with number-right-direction = 0
     with number-wrong-direction = 0
     finally (format t "~&Right direction = ~A Wrong direction = ~A" number-right-direction number-wrong-direction)
     do
     (if (if (>= (unscore (node-output test-set (- i start-index)) *array* :function-name #'+5close-diff)
		 0)		      ;If predicted change is positive
	     (>= (closeb (aref *array* (+ i 5)))
		 (closeb (aref *array* i))) ;Is future-close > current-close?
	     (< (closeb (aref *array* (+ i 5)))
		(closeb (aref *array* i)))) ;Is future-close < current-close?
	 (incf number-right-direction)
	 (incf number-wrong-direction))))

(defun aggregate-error-in-set (dataset)
  "This function will find out what the total error is in the training set compared to the currect network output."
  (loop for i from 0 below (array-dimension dataset 0)
     sum (abs      (- (aref dataset i 2)	      ;Answer we want
		      (node-output dataset i))))) ;Answer we have right now.
