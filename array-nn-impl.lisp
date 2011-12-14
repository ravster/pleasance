;; Copyright 2011 Ravi Desai
;; Distributed under the GNU General Public License version 3 or later.

;; Set the package
(in-package :pleasance)

;; State number of input and hidden nodes
(defparameter numberofinputnodes 3)
(defparameter numberofhiddennodes 7)

;; Define weights for 1st interlevel (Between input and hidden nodes)
(defparameter weights-1 (make-array (list numberofinputnodes numberofhiddennodes)))
(defun define-weights-1 ()
  (loop for i below (array-dimension weights-1 0) do
       (loop for j below (array-dimension weights-1 1) do
	    (setf (aref weights-1 i j) (random 3)))))
(define-weights-1)

;; Define weights for 2nd interlevel
(defparameter weights-2 (make-array numberofhiddennodes))
(defun define-weights-2 ()
  (loop for i below (length weights-2) do
       (setf (aref weights-2 i) (random 3))))
(define-weights-2)

;; Define rate of learning
(defparameter rate-of-learning (/ (1+ (random 10))
				     1000))

;; Define input-nodes
(defparameter input-node (make-array numberofinputnodes))

(defun define-input-nodes (list-of-set-indexes)
  "Takes a list of set indices that refer to particular indicators in the sets and maps only those indicators into the input-nodes of the NN.  The indices are zero-based and refer to the column vectors of the sets."
  (loop for j below (length input-node)
     do
     (let ((i (elt list-of-set-indexes j)))
       (setf (aref input-node j)
	     (lambda (dataset input-index)
	       (aref dataset input-index i))))))

(define-input-nodes '(1 11 12))

;; Define hidden nodes
(defparameter hidden-node (make-array numberofhiddennodes))

(defun define-hidden-nodes ()
  (loop for i below numberofhiddennodes do
       (let ((hidden-node-index i) ;Because we are closing over this variable for each hidden-node.
	     (number-of-input-nodes numberofinputnodes))
	 (setf (aref hidden-node hidden-node-index)
	       (lambda (dataset input-index)
		 (tanh
		  (loop for input-node-index below number-of-input-nodes sum
		       (* (aref weights-1 input-node-index hidden-node-index)
			  (funcall (aref input-node input-node-index)
				   dataset input-index)))))))))

(define-hidden-nodes)

;; Define output node.  Same name and parameters used right now so that it plugs into the rest of the code.

(defun node-output (dataset input-index)
  (tanh
   (loop for i below numberofhiddennodes sum (* (aref weights-2 i) 
						(funcall (aref hidden-node i) dataset input-index)))))

;; The following code is used for training the NN.
(defparameter hneg (make-array numberofhiddennodes))
(defun error-gradient-of-output-node (dataset input-index)
  ""
  (* (- 1 (expt (node-output dataset input-index)
		2))			     ;1 - y^2
     (- (aref training-set input-index 2)    ;Answer we want
	(node-output dataset input-index)))) ;Answer we have right now.

(defun work-horse (dataset)
  "This trains the NN.  This counts as 1 epoch."
  (loop for exemplar below (array-dimension dataset 0)
       with output-node-error-gradient = 0
       do
       (setf output-node-error-gradient (error-gradient-of-output-node dataset exemplar))
     ;; Calc hiddennode error gradients.  For use with updating weights-1
       (loop for i below numberofhiddennodes	;For all hidden nodes.
	    do
	    (setf (aref hneg i)
		  (* (aref weights-2 i)	;Weight of the hidden node
		     (- 1 (expt (funcall (aref hidden-node i) dataset exemplar)
				2))	;Derivative of output of hidden node.
		     output-node-error-gradient)))

     ;; Update weights-1
       (loop for input-node-iterator below numberofinputnodes do
	    (loop for hidden-node-iterator below numberofhiddennodes do
		 (incf (aref weights-1 input-node-iterator hidden-node-iterator)
		       (* rate-of-learning
			  (funcall (aref input-node input-node-iterator) dataset exemplar)
			  (aref hneg hidden-node-iterator)))))

     ;; Update weights-2.
     ;; We do this after weights-1 to maintain integrity.  Weights-1 is changed on the old weights-2 values, and weights-2 does not depend upon weights-1.
       (loop for i below numberofhiddennodes
	    do
	    (incf (aref weights-2 i)
		  (* rate-of-learning
		     (funcall (aref hidden-node i) dataset exemplar) ;Output of hidden node.
		     output-node-error-gradient)))
       ))