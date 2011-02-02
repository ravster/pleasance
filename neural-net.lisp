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

;; Define rate of learning

(defparameter rate-of-learning (/ (1+ (random 10))
				     1000))

;; Define the nodes

(defun node-i0 (input-index)
  ""
  (aref training-set input-index 0))

(defun node-i1 (input-index)
  ""
  (aref training-set input-index 1))

(defun node-h0 (input-index)
  ""
  (tanh (+ (* weight-i0h0 (node-i0 input-index))
	   (* weight-i1h0 (node-i1 input-index)))))

(defun node-h1 (input-index)
  ""
  (tanh (+ (* weight-i0h1 (node-i0 input-index))
	   (* weight-i1h1 (node-i1 input-index)))))

(defun node-output (input-index)
  ""
  (tanh (+ (* weight-h0 (node-h0 input-index))
	   (* weight-h1 (node-h1 input-index)))))

(defun error-gradient-of-output-node (input-index)
  ""
  (* (- 1 (expt (node-output input-index)
		2))			  ;1 - y^2
     (- (aref training-set input-index 2) ;Answer we want
	(node-output input-index))))	  ;Answer we have right now.

(defun work-horse ()
  ""
  (loop for i from 0 below 5000	;Due to 5000 datum in the training set.
     do
     ;; Update the weights to the output layer.
       (incf weight-h0 (+ rate-of-learning
			  (node-h0 i)
			  (error-gradient-of-output-node i)))

       (incf weight-h1 (+ rate-of-learning
			  (node-h1 i)
			  (error-gradient-of-output-node i)))

     ;; Update the weights to the hidden layer.
       (incf weight-i0h0 (+ rate-of-learning
			    (node-i0 i)
			    (* weight-h0 ;This is the error gradient
			       (error-gradient-of-output-node i))))
       (incf weight-i1h0 (+ rate-of-learning
			    (node-i1 i)
			    (* weight-h0 ;Error gradient for H0
			       (error-gradient-of-output-node i))))
       (incf weight-i0h1 (+ rate-of-learning
			    (node-i0 i)
			    (* weight-h1 ;Error gradient for H1
			       (error-gradient-of-output-node i))))
       (incf weight-i1h1 (+ rate-of-learning
			    (node-i1 i)
			    (* weight-h1 ;Error gradient for H1
			       (error-gradient-of-output-node i))))
       ))				;End loop and defun

