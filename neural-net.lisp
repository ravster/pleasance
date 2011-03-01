;;;; The goal is to make a net that can predict when a big move is about to happen (As defined by a higher-than-normal TR).

(in-package :ravi.nn0)

;; Define weights
(defparameter weight-i0h0 (random 3))
(defparameter weight-i0h1 (random 3))
(defparameter weight-i0h2 (random 3))
(defparameter weight-i0h3 (random 3))
(defparameter weight-i0h4 (random 3))

(defparameter weight-i1h0 (random 3))
(defparameter weight-i1h1 (random 3))
(defparameter weight-i1h2 (random 3))
(defparameter weight-i1h3 (random 3))
(defparameter weight-i1h4 (random 3))

(defparameter weight-i2h0 (random 3))
(defparameter weight-i2h1 (random 3))
(defparameter weight-i2h2 (random 3))
(defparameter weight-i2h3 (random 3))
(defparameter weight-i2h4 (random 3))

(defparameter weight-h0 (random 3))
(defparameter weight-h1 (random 3))
(defparameter weight-h2 (random 3))
(defparameter weight-h3 (random 3))
(defparameter weight-h4 (random 3))

;; Define rate of learning

(defparameter rate-of-learning (/ (1+ (random 10))
				     1000))

;; Define the nodes

(defun node-i0 (dataset input-index)
  "Score of (MA - closing price)."
  (aref dataset input-index 0))

(defun node-i1 (dataset input-index)
  "Score of ATR."
  (aref dataset input-index 1))

(defun node-i2 (dataset input-index)
  "Score of ADX."
  (aref dataset input-index 3))

(defun node-h0 (dataset input-index)
  ""
  (tanh (+ (* weight-i0h0 (node-i0 dataset input-index))
	   (* weight-i1h0 (node-i1 dataset input-index))
	   (* weight-i2h0 (node-i2 dataset input-index)))))

(defun node-h1 (dataset input-index)
  ""
  (tanh (+ (* weight-i0h1 (node-i0 dataset input-index))
	   (* weight-i1h1 (node-i1 dataset input-index))
	   (* weight-i2h1 (node-i2 dataset input-index)))))

(defun node-h2 (dataset input-index)
  ""
  (tanh (+ (* weight-i0h2 (node-i0 dataset input-index))
	   (* weight-i1h2 (node-i1 dataset input-index))
	   (* weight-i2h2 (node-i2 dataset input-index)))))

(defun node-h3 (dataset input-index)
  ""
  (tanh (+ (* weight-i0h3 (node-i0 dataset input-index))
	   (* weight-i1h3 (node-i1 dataset input-index))
	   (* weight-i2h3 (node-i2 dataset input-index)))))

(defun node-h4 (dataset input-index)
  ""
  (tanh (+ (* weight-i0h4 (node-i0 dataset input-index))
	   (* weight-i1h4 (node-i1 dataset input-index))
	   (* weight-i2h4 (node-i2 dataset input-index)))))

(defun node-output (dataset input-index)
  ""
  (tanh (+ (* weight-h0 (node-h0 dataset input-index))
	   (* weight-h1 (node-h1 dataset input-index))
	   (* weight-h2 (node-h2 dataset input-index))
	   (* weight-h3 (node-h3 dataset input-index))
	   (* weight-h4 (node-h4 dataset input-index)))))
;)))

(defun error-gradient-of-output-node (dataset input-index)
  ""
  (* (- 1 (expt (node-output dataset input-index)
		2))			  ;1 - y^2
     (- (aref training-set input-index 2) ;Answer we want
	(node-output dataset input-index))))	  ;Answer we have right now.

(defmacro update-weight-from-hidden-node (node weight-from-node dataset incrementor)
  "Macro to increment the weight from the given hidden node."
  `(incf ,weight-from-node (* rate-of-learning
			      (,node ,dataset ,incrementor)
			      (error-gradient-of-output-node ,dataset ,incrementor))))

(defmacro update-weight-from-input-node (input-node hidden-node input-node-weight hidden-node-weight dataset incrementor)
  "Macro to increment the weight from the given input node."
  `(incf ,input-node-weight (* rate-of-learning
			       (,input-node ,dataset ,incrementor)
			       ,hidden-node-weight ;Error gradient for hidden node.
			       (- 1 (expt (,hidden-node ,dataset ,incrementor)
					  2)) ;1 - y^2
			       (error-gradient-of-output-node ,dataset ,incrementor))))

(defun work-horse (dataset)
  "This function does the learning.  It updates the weights amongst the nodes."
  (loop for i from 0 below (array-dimension dataset 0) ;Since dataset is not a vector, but a 2-dimensional array.
     do
     ;; Update the weights to the output layer.
       (update-weight-from-hidden-node node-h0 weight-h0 dataset i)
       (update-weight-from-hidden-node node-h1 weight-h1 dataset i)
       (update-weight-from-hidden-node node-h2 weight-h2 dataset i)
       (update-weight-from-hidden-node node-h3 weight-h3 dataset i)
       (update-weight-from-hidden-node node-h4 weight-h4 dataset i)

     ;; Update the weights to the hidden layer.
       (update-weight-from-input-node node-i0 node-h0 weight-i0h0 weight-h0 dataset i)
       (update-weight-from-input-node node-i0 node-h1 weight-i0h1 weight-h1 dataset i)
       (update-weight-from-input-node node-i0 node-h2 weight-i0h2 weight-h2 dataset i)
       (update-weight-from-input-node node-i0 node-h3 weight-i0h3 weight-h3 dataset i)
       (update-weight-from-input-node node-i0 node-h4 weight-i0h4 weight-h4 dataset i)

       (update-weight-from-input-node node-i1 node-h0 weight-i1h0 weight-h0 dataset i)
       (update-weight-from-input-node node-i1 node-h1 weight-i1h1 weight-h1 dataset i)
       (update-weight-from-input-node node-i1 node-h2 weight-i1h2 weight-h2 dataset i)
       (update-weight-from-input-node node-i1 node-h3 weight-i1h3 weight-h3 dataset i)
       (update-weight-from-input-node node-i1 node-h4 weight-i1h4 weight-h4 dataset i)

       (update-weight-from-input-node node-i2 node-h0 weight-i2h0 weight-h0 dataset i)
       (update-weight-from-input-node node-i2 node-h1 weight-i2h1 weight-h1 dataset i)
       (update-weight-from-input-node node-i2 node-h2 weight-i2h2 weight-h2 dataset i)
       (update-weight-from-input-node node-i2 node-h3 weight-i2h3 weight-h3 dataset i)
       (update-weight-from-input-node node-i2 node-h4 weight-i2h4 weight-h4 dataset i)

       ))				;End loop and defun

(defun aggregate-error-in-set (dataset)
  "This function will find out what the total error is in the training set compared to the currect network output."
  (loop for i from 0 below (array-dimension dataset 0)
     sum (abs      (- (aref dataset i 2)	      ;Answer we want
		      (node-output dataset i))))) ;Answer we have right now.

(defun hit? (start-index end-index)
  "Does the price ever hit the predicted values in the next 5 periods?"
  (loop for i from start-index below end-index	;over the test-set
     with number-of-hits = 0
     with number-of-misses = 0
     with predicted-price = 0
     with predicted-change = 0
     finally (format t "~&Number of hits = ~A Number of misses = ~A" number-of-hits number-of-misses)
     do
     (setf predicted-change (unscore (node-output test-set (- i start-index)) *array* :function-name #'+5close-diff))
     (setf predicted-price (+ (closeb (aref *array* i)) ;What the price is at present.
			      predicted-change))

     (if (if (>= predicted-change 0)
	     (>= (loop for j from (1+ i) upto (+ i 5) ;Because the trade is really taken after the i-th bar ends.
		    maximize (high (aref *array* j)))
		 predicted-price) ;Does the price go as high as the predicted price?
	     (< (loop for j from (1+ i) upto (+ i 5)
		   minimize (low (aref *array* j)))
		predicted-price)) ;Does the price go as low as the predicted price?
	 (incf number-of-hits)
	 (incf number-of-misses))))

(defun right-direction? (start-index end-index)
  "Did the NN predict the correct direction of the market for the next 5 periods?"
  (loop for i from start-index below end-index
     with number-right-direction = 0
     with number-wrong-direction = 0
     finally (format t "~&Right direction = ~A Wrong direction = ~A" number-right-direction number-wrong-direction)
     do
     (if (if (>= (unscore (node-output test-set (- i start-index)) *array* :function-name #'+5close-diff)
		 0)			;If predicted change is positive
	     (>= (closeb (aref *array* (+ i 5)))
		 (closeb (aref *array* i))) ;Is future-close > current-close?
	     (< (closeb (aref *array* (+ i 5)))
		(closeb (aref *array* i)))) ;Is future-close < current-close?
	 (incf number-right-direction)
	 (incf number-wrong-direction))))
