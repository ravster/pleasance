;;;; This is an NN module for use by the GA.  We give the NN module a chromosome of binary genes defining which input-vectors to use.  The NN module creates a NN, trains it, and then returns the error for the training and the testing sets back to the GA
;; This will be all in one defun, and I will be using labels to have locally-defined functions.  I will be making arrays of the weights and nodes to make it flexible from iteration to iteration of the GA.

(in-package :ravi.nn0)

(defun nn (chromosome)
  (let* ((numberofinputnodes (loop for i in chromosome count (= i 1)))
	 (numberofhiddennodes (* 2 numberofinputnodes))
	 (weights-1 (make-array (list numberofinputnodes numberofhiddennodes)))
	 (weights-2 (make-array numberofhiddennodes))
	 (rate-of-learning (/ 3 1000))
	 (input-node (make-array numberofinputnodes))
	 (hidden-node (make-array numberofhiddennodes))
	 (hneg (make-array numberofhiddennodes)))
    (labels ((error-gradient-of-output-node (dataset input-index)
	       (* (- 1 (expt (output-node dataset input-index)
			     2))		       ;1 - y^2
		  (- (aref training-set input-index 2) ;Answer we want
		     (output-node dataset input-index)))) ;Answer we have right now.
	     (output-node (dataset input-index)
	       (tanh
		(loop for i below numberofhiddennodes sum (* (aref weights-2 i) 
							     (funcall (aref hidden-node i) dataset input-index)))))
	     (work-horse (dataset)
	       (loop for exemplar below (array-dimension dataset 0) ;For the length of the dataset.
		  with output-node-error-gradient = 0
		  do
		  (setf output-node-error-gradient (error-gradient-of-output-node dataset exemplar))
		  ;; Calc hiddennode error gradients.  For use with updating weights-1
		  (loop for i below (length hidden-node) ;For all hidden nodes.
		     do
		     (setf (aref hneg i)
			   (* (aref weights-2 i) ;Weight of the hidden node
			      (- 1 (expt (funcall (aref hidden-node i) dataset exemplar)
					 2)) ;Derivative of output of hidden node.
			      output-node-error-gradient)))

		  ;; Update weights-1
		  (loop for input-node-iterator below (array-dimension weights-1 0) do
		       (loop for hidden-node-iterator below (array-dimension weights-1 1) do
			    (incf (aref weights-1 input-node-iterator hidden-node-iterator)
				  (* rate-of-learning
				     (funcall (aref input-node input-node-iterator) dataset exemplar)
				     (aref hneg hidden-node-iterator)))))

		  ;; Update weights-2.
		  ;; We do this after weights-1 to maintain integrity.  Weights-1 is changed on the hneg of the old weights-2 values, and weights-2 does not depend upon weights-1.
		  (loop for i below (length hidden-node)
		     do
		     (incf (aref weights-2 i)
			   (* rate-of-learning
			      (funcall (aref hidden-node i) dataset exemplar) ;Output of hidden node.
			      output-node-error-gradient)))
		  ))			;End work-horse definition.
	     (chromosome-to-input-vectors (chromosome)
	       (let ((a ()))
		 (loop for i below (length chromosome) do
		      (if (= 1 (elt chromosome i))
			  (push (1+ i)
				a)))
		 a))
	     (define-input-nodes (list-of-set-indexes)
	       (loop for j below (length input-node)
		  do
		  (let ((i (elt list-of-set-indexes j)))
		    (setf (aref input-node j)
			  (lambda (dataset input-index)
			    (aref dataset input-index i))))))
	     (define-hidden-nodes ()
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
	     (aggregate-error-in-set (dataset)
	       "This function will find out what the total error is in the training set compared to the currect network output."
	       (loop for i from 0 below (array-dimension dataset 0)
		  sum (abs (- (aref dataset i 0) ;Answer we want
			      (output-node dataset i))))) ;Answer we have right now.
	     (directional-accuracy-in-set (dataset)
	       "This function finds out accuracy of directional forecasts."
	       (loop for i from 5 below (array-dimension dataset 0)
		    with right = 0
		    and wrong = 0 do
		    (if (plusp (* (- (aref dataset i 0) ;What it is
				     (aref dataset (- i 5) 0)) ;What it was
				  (- (output-node dataset i)   ;What we got
				     (aref dataset (- i 5) 0)))) ;What it was
			(incf right)
			(incf wrong))
		    finally (return (/ right (+ right wrong)))))
	     )				;End labels definition
	
      ;; Create the input-nodes
      (define-input-nodes (chromosome-to-input-vectors chromosome))

      ;; Create the hidden-nodes
      (define-hidden-nodes)

      ;; Define weights-1; the first interlevel
      (loop for i below (array-dimension weights-1 0) do
	   (loop for j below (array-dimension weights-1 1) do
		(setf (aref weights-1 i j) (random 3))))

      ;; Define weights-2; the second interlevel
      (loop for i below (length weights-2) do
	   (setf (aref weights-2 i) (random 3)))

      ;; Train the network till the validation-set error begins to go up.

      (let ((previous-validation-error))
	(loop for i below 2000
	   initially 
	   (work-horse training-set)	
	   (setf previous-validation-error (aggregate-error-in-set validation-set)) ;To get the first aggregate error amount.
	   finally (return (list
			    (aggregate-error-in-set training-set)
			    (aggregate-error-in-set test-set)
			    (directional-accuracy-in-set test-set)))
	   do
	   (work-horse training-set)
	   (if (< (aggregate-error-in-set validation-set)
		  previous-validation-error) ;If the new error is less than the old error, set old error to new-error and continue with the next iteration.
	       (setf previous-validation-error (aggregate-error-in-set validation-set)) ;Update the aggregate error amount.
	       (loop-finish))))		;End the loop

      
	
      ))
  )			       ;End labels, macrolet, let*, and defun.
