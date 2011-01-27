;;;; The goal is to make a net that can predict when a big move is about to happen (As defined by a higher-than-normal TR).

(in-package :ravi.nn0)

;; Define weights
(defparameter weight-i0h0 (random 3)
  "From atr-node to first hidden node.")

(defparameter weight-i0h1 (random 3))

(defparameter weight-i1h0 (random 3))

(defparameter weight-i1h1 (random 3))

;; Define rate of learning

(defparameter rate-of-learning (/ (1+ (random 10))
				     1000))

(defun node-i0 (input)
  ""
  (tanh (* input
	   weight-i0h0)))

(defun node-h0 (input)
  ""
  (tanh (* input
	   weight-h0o0)))

(defun main-run ()
  ""
  (let ((output-i0)
	(output-h0))
    (format t "~&weight-i0: ~A~&weight-h0: ~A~&------------" weight-i0h0 weight-h0o0)
    (loop for i from 0 below 5000	;We have 5000 data-points in the training set.
	 do
	 (setf output-i0 (node-i0 (aref training-set i 0))
	       output-h0 (node-h0 output-i0))
       ;; Saved the outputs of the nodes.
	 (incf weight-h0o0 (* rate-of-learning-h0
			     (- (aref training-set i 2)
				output-h0)))
       ;; Increment the weight by the rate of learning times (What the output should have been) minus (What the output was).
       ;; I don't think doing a simple subtraction & multiplication is wise, since we are talking of a hyperbolic function here and the input itself could be either positive or negative.
	 (incf weight-i0h0 (* rate-of-learning-i0
			    (- (aref training-set i 2)
			       output-h0)))
       ;; Same as above.
       ;; Also, I've used the difference in the outputs here since I don't know how to use anything else (For now).  I could use a group of people who are willing _and_ able to be working on this with me.  A team (Small one) would be quite good right now.
	 )
    (format t "~&weight-i0: ~A~&weight-h0: ~A" weight-i0h0 weight-h0o0)))