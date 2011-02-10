;;;; The goal is to make a net that can predict when a big move is about to happen (As defined by a higher-than-normal TR).

(in-package :ravi.nn0)

;; Define weights
(defparameter weight-i0h0 (random 3)
  "From atr-node to first hidden node.")

(defparameter weight-i0h1 (random 3))

(defparameter weight-i1h0 (random 3))

(defparameter weight-i1h1 (random 3))

(defparameter weight-h0 (random 3))

(defparameter weight-h1 (random 3))

(defparameter weight-i0h2 (random 3))
(defparameter weight-i1h2 (random 3))
(defparameter weight-h2 (random 3))

;; Define rate of learning

(defparameter rate-of-learning (/ (1+ (random 10))
				     1000))

;; Define the nodes

(defun node-i0 (input-index)
  "Score of (MA - closing price)."
  (aref training-set input-index 0))

(defun node-i1 (input-index)
  "Score of ATR."
  (aref training-set input-index 1))

(defun node-h0 (input-index)
  ""
  (tanh (+ (* weight-i0h0 (node-i0 input-index))
	   (* weight-i1h0 (node-i1 input-index)))))

(defun node-h1 (input-index)
  ""
  (tanh (+ (* weight-i0h1 (node-i0 input-index))
	   (* weight-i1h1 (node-i1 input-index)))))

(defun node-h2 (input-index)
  ""
  (tanh (+ (* weight-i0h2 (node-i0 input-index))
	   (* weight-i1h2 (node-i1 input-index)))))

(defun node-output (input-index)
  ""
  (tanh (+ (* weight-h0 (node-h0 input-index))
	   (* weight-h1 (node-h1 input-index))
	   (* weight-h2 (node-h2 input-index)))))

(defun error-gradient-of-output-node (input-index)
  ""
  (* (- 1 (expt (node-output input-index)
		2))			  ;1 - y^2
     (- (aref training-set input-index 2) ;Answer we want
	(node-output input-index))))	  ;Answer we have right now.

(defun work-horse ()
  "This function does the learning.  It updates the weights amongst the nodes."
  (loop for i from 0 below 5000	;Due to 5000 datum in the training set.
     do
     ;; Update the weights to the output layer.
       (incf weight-h0 (* rate-of-learning
			  (node-h0 i)
			  (error-gradient-of-output-node i)))

       (incf weight-h1 (* rate-of-learning
			  (node-h1 i)
			  (error-gradient-of-output-node i)))

       (incf weight-h2 (* rate-of-learning
			  (node-h2 i)
			  (error-gradient-of-output-node i)))

     ;; Update the weights to the hidden layer.
       (incf weight-i0h0 (* rate-of-learning
			    (node-i0 i)
			    (* weight-h0 ;This is the error gradient
			       (- 1 (expt (node-h0 i)
					  2))
			       (error-gradient-of-output-node i))))
       (incf weight-i1h0 (* rate-of-learning
			    (node-i1 i)
			    (* weight-h0 ;Error gradient for H0
			       (- 1 (expt (node-h0 i)
					  2))
			       (error-gradient-of-output-node i))))
       (incf weight-i0h1 (* rate-of-learning
			    (node-i0 i)
			    (* weight-h1 ;Error gradient for H1
			       (- 1 (expt (node-h1 i)
					  2))
			       (error-gradient-of-output-node i))))
       (incf weight-i1h1 (* rate-of-learning
			    (node-i1 i)
			    (* weight-h1 ;Error gradient for H1
			       (- 1 (expt (node-h1 i)
					  2))
			       (error-gradient-of-output-node i))))
       (incf weight-i0h2 (* rate-of-learning
			    (node-i0 i)
			    (* weight-h2 ;Error gradient for H2
			       (- 1 (expt (node-h2 i)
					  2))
			       (error-gradient-of-output-node i))))
       (incf weight-i1h2 (* rate-of-learning
			    (node-i1 i)
			    (* weight-h2 ;Error gradient for H2
			       (- 1 (expt (node-h2 i)
					  2))
			       (error-gradient-of-output-node i))))

       ))				;End loop and defun

(defun aggregate-error-in-training-set ()
  "This function will find out what the total error is in the training set compared to the currect network output."
  (loop for i from 0 below 5000
     sum (abs      (- (aref training-set i 2) ;Answer we want
			  (node-output i))))) ;Answer we have right now.
