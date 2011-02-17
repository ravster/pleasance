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

(defun node-h0 (dataset input-index)
  ""
  (tanh (+ (* weight-i0h0 (node-i0 dataset input-index))
	   (* weight-i1h0 (node-i1 dataset input-index)))))

(defun node-h1 (dataset input-index)
  ""
  (tanh (+ (* weight-i0h1 (node-i0 dataset input-index))
	   (* weight-i1h1 (node-i1 dataset input-index)))))

(defun node-h2 (dataset input-index)
  ""
  (tanh (+ (* weight-i0h2 (node-i0 dataset input-index))
	   (* weight-i1h2 (node-i1 dataset input-index)))))

(defun node-h3 (dataset input-index)
  ""
  (tanh (+ (* weight-i0h3 (node-i0 dataset input-index))
	   (* weight-i1h3 (node-i1 dataset input-index)))))

(defun node-h4 (dataset input-index)
  ""
  (tanh (+ (* weight-i0h4 (node-i0 dataset input-index))
	   (* weight-i1h4 (node-i1 dataset input-index)))))

(defun node-output (dataset input-index)
  ""
  (tanh (+ (* weight-h0 (node-h0 dataset input-index))
	   (* weight-h1 (node-h1 dataset input-index))
	   (* weight-h2 (node-h2 dataset input-index))
;	   (* weight-h3 (node-h3 dataset input-index))
;	   (* weight-h4 (node-h4 dataset input-index)))))
)))

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
  (loop for i from 0 below 5000	;Due to 5000 datum in the training set.
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

       ))				;End loop and defun

(defun aggregate-error-in-set (dataset)
  "This function will find out what the total error is in the training set compared to the currect network output."
  (loop for i from 0 below 5000
     sum (abs      (- (aref dataset i 2)	      ;Answer we want
		      (node-output dataset i))))) ;Answer we have right now.
