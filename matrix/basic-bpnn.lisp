;; Set the package
(in-package :ravi.matrix0)

;; Number of inputs and outputs and hidden nodes
(defparameter numberofinputnodes 5)
(defparameter numberofhiddennodes 10)
(defparameter numberofoutputnodes 1)

;; Array for input data
(defparameter input-data (grid:make-foreign-array 'single-float :dimensions `(,(- (length *array*) 50) ,(1+ numberofinputnodes))))

;; Function to create scores.  Seperate from RAVI.NN0 because it does it directly to the foreign-array this way.
(defun create-scores (bar-array output-array &key (min-output -1) (range-output 2) function-name (index-shift 50) (start-index 0) (end-index 5000) output-array-index)
  "Use the min-max normalization to create scores of input data for the neural network."
  (loop for i below (- end-index start-index)
     with max-input = (loop for j from (+ start-index index-shift) below (+ end-index index-shift)
			 maximize (funcall function-name (aref bar-array j)))
     with min-input = (loop for j from (+ start-index index-shift) below (+ end-index index-shift)
			 minimize (funcall function-name (aref bar-array j)))
     with range-input = (- max-input min-input)
     do
       (setf (grid:gref output-array i output-array-index)
	     (ravi.nn0:score (funcall function-name (aref bar-array (+ i index-shift))) 
			     :min-output min-output :range-output range-output :min-input min-input :range-input range-input))))

;; Populate the input-data foreign array
(loop for i below (- (length *array*) 50) do
     (setf (grid:gref input-data i 0) 1.0))
(create-scores *array* input-data :function-name #'atrb :output-array-index 1 :start-index 0 :end-index 3900)
(create-scores *array* input-data :function-name #'mso :output-array-index 2 :start-index 0 :end-index 3900)
(create-scores *array* input-data :function-name #'adx :output-array-index 3 :start-index 0 :end-index 3900)
(create-scores *array* input-data :function-name #'momentum :output-array-index 4 :start-index 0 :end-index 3900)
(create-scores *array* input-data :function-name #'roc :output-array-index 5 :start-index 0 :end-index 3900)

;; Array for output data
(defparameter output-data (grid:make-foreign-array 'single-float :dimensions `(,(- (length *array*) 50) ,numberofoutputnodes)))

;; Populate the output array
(create-scores *array* output-data :function-name #'+5close-diff :output-array-index 0 :start-index 0 :end-index 3900)

;; Function to provide 1 observation from the input-data, ordered by 'pattern-index'.
(defun x (pattern-index)
  "Provide the index in INPUT-DATA for the pattern that you want.  This is a row-vector."
  (row input-data pattern-index))

(defun x-matrix (pattern-index)
  "Returns a '(1+ numberofinputnodes) x 1' column-vector of the pattern-index."
  (let ((x0 (x pattern-index))
	(x1 (grid:make-foreign-array 'single-float :dimensions `(,(1+ numberofinputnodes) 1))))
    (loop for i upto numberofinputnodes
       finally (return x1) do
	 (setf (grid:gref x1 i 0)
	       (grid:gref x0 i)))))

(defun y (pattern-index)
  "Row-vector of the output for a particular pattern."
  (row output-data pattern-index))

;; Foreign array of weights from input to hidden layer
(defparameter w (grid:make-foreign-array 'single-float :dimensions `(,numberofhiddennodes ,(1+ numberofinputnodes))))
(loop for i below numberofhiddennodes do
     (loop for j upto numberofinputnodes do
	  (setf (grid:gref w i j) (random 1.0462))))

;; Foreign array of weights from hidden to output layer
(defparameter v (grid:make-foreign-array 'single-float :dimensions `(,(1+ numberofhiddennodes) ,numberofoutputnodes)))
(loop for i upto numberofhiddennodes do
     (loop for j below numberofoutputnodes do
	  (setf (grid:gref v i j) (random 1.0736))))

;; Function to represent the output of the hidden layer for a given pattern (Observation of data).
(defun f1 (pattern-index)
  "This function takes the pattern-index and finds out the output of the hidden layer for the pattern-indexed pattern in the INPUT-DATA.
The output of this function is a vector of length (1+ numberofhiddennodes) to reflect the fact that the 0-th element is a 1.0 and used as a bias for the output layer."
  (let ((v1 (matrix-product w (x pattern-index)))
	;; V1 holds the weighted sums of the hidden-nodes
	;; FinalV holds the output of the hidden layer (With the bias element)
	(finalv (grid:make-foreign-array 'single-float :dimensions (1+ numberofhiddennodes) :initial-element 1.0)))
    ;; Apply the tanh function to V1 to get the _output_ of the hidden-nodes.
    (loop for i below numberofhiddennodes do
	 (setf (grid:gref v1 i)
	       (tanh (grid:gref v1 i))))

    ;; Move everything to FinalV and return FinalV
    (loop for i from 1 upto numberofhiddennodes
       finally (return finalv) do
	 (setf (grid:gref finalv i)
	       (grid:gref v1 (1- i))))))

;; Function to represent the output of the output layer for a given observation of input-data
(defun f2 (pattern-index)
  "This function takes the pattern-index and finds out the final output of the NN for that pattern from the INPUT-DATA."
  (let ((m1 (matrix-product (matrix-transpose v) (f1 pattern-index))))
    ;; Apply tanh to get the output of the output layer and the NN as a whole.
    (loop for i below numberofoutputnodes
       finally (return m1) do
	 (setf (grid:gref m1 i)
	       (tanh (grid:gref m1 i))))))

;; Define the learning rate as a function
(defun learning-rate ()
  0.3)

;; Define error
(defun nn-error (pattern-index)
  "This function returns the error-vector for the pattern 'pattern-index'.  It gives us a vector of the actual minus predicted y-values."
  (let ((error-vector (grid:make-foreign-array 'single-float :dimensions numberofoutputnodes))
	(f2 (f2 pattern-index)))	;Output of the NN.
    (blas-copy (y pattern-index) error-vector)
    ;; Copy y into error-vector since there is no functional way to do an element-wise vector subtraction.
    (elt- error-vector f2)))		;Reduce 'what we should get' by 'what we did get'.

;; Truncate matrix V so that it doesn't have the first row
(defun truncated-v ()
  "Returns the V-matrix without the first row.  That first row is for the bias term, and there is no need for that when calculating the change in the W-matrix"
  (loop for i below numberofhiddennodes
     with truncated-v = (grid:make-foreign-array 'single-float :dimensions `(,numberofhiddennodes ,numberofoutputnodes))
     finally (return truncated-v) do
       (setf (row truncated-v i)
	     (row v (1+ i)))))

(defun f1-prime (pattern-index)
  "A diagonal square matrix of size 'numberofhiddennodes', consisting of the derivative of the output of the hidden layer for the pattern-index"
  (loop for i below numberofhiddennodes
     with f1 = (f1 pattern-index)
     and f1-prime = (grid:make-foreign-array 'single-float :dimensions `(,numberofhiddennodes ,numberofhiddennodes) :initial-element 0)
     finally (return f1-prime) do
       (setf (grid:gref f1-prime i i)
	     (- 1 (expt (grid:gref f1 (1+ i))
			2)))))

(defun f2-prime (pattern-index)
  "A diagonal square matrix of size 'numberofoutputnodes', of the derivative of the output of the output layer for the pattern-index"
  (loop with f2-prime = (grid:make-foreign-array 'single-float :dimensions `(,numberofoutputnodes ,numberofoutputnodes) :initial-element 0)
     and f2 = (f2 pattern-index)	;Output of NN.
     for i below numberofoutputnodes
     finally (return f2-prime) do
       (setf (grid:gref f2-prime i i)
	     (- 1 (expt (grid:gref f2 i)
			2)))))

(defun change-in-w (start-index end-index)
  "This is delta-W for 1 epoch."
  (let ((m0 (grid:make-foreign-array 'single-float :dimensions numberofhiddennodes))
	(m1 (grid:make-foreign-array 'single-float :dimensions `(,numberofhiddennodes 1)))
	(matrix-sum (grid:make-foreign-array 'single-float :dimensions `(,numberofhiddennodes ,(1+ numberofinputnodes)) :initial-element 0.0))) ;q * (p + 1)
    (loop for pattern-index from start-index below end-index 
       do
	 (setf m0 (matrix-product (matrix-product (matrix-product (f1-prime pattern-index) (truncated-v)) (f2-prime pattern-index)) (nn-error pattern-index)))
	 (loop for i below numberofhiddennodes do
	      (setf (grid:gref m1 i 0)
		    (grid:gref m0 i)))
	 (elt+ matrix-sum
	       (matrix-product m1 (matrix-transpose (x-matrix pattern-index)))))
    ;; Now we multiply ever element of the matrix-sum by the negative of the learning-rate.
    ;; Should figure out how to do this using GSLL.  The elt* function doesn't seem to work.
    (loop for i below numberofhiddennodes
       for j below (1+ numberofinputnodes) 
       finally (return matrix-sum) do
	 (setf (grid:gref matrix-sum i j)
	       (* (grid:gref matrix-sum i j)
		  (* -1 (learning-rate)))))))

(defun matrixify (foreign-vector length-of-vector)
  "Turn a vector into a row vector in a matrix-form.  This is due to limitations in the GSL w.r.t. outer products of vectors and transposing of vectors."
  (let ((foreign-matrix (grid:make-foreign-array 'single-float :dimensions `(1 ,length-of-vector))))
    (blas-copy foreign-vector (row foreign-matrix 0))
    foreign-matrix))

(defun change-in-v (start-index end-index)
  "This is delta-V for 1 epoch."
  (let ((matrix-sum (grid:make-foreign-array 'single-float :dimensions `(,(1+ numberofhiddennodes) ,numberofoutputnodes) :initial-element 0.0)))
    (loop for pattern-index from start-index below end-index do
	 (elt+ matrix-sum
	       (matrix-product (matrix-product (matrix-transpose (matrixify (f1 pattern-index) 
									    (1+ numberofhiddennodes))) 
					       (matrixify (nn-error pattern-index) 
							  numberofoutputnodes))
			       (f2-prime pattern-index))))
    (loop for i upto numberofhiddennodes
       for j below numberofoutputnodes
       finally (return matrix-sum) do
	 (setf (grid:gref matrix-sum i j)
	       (* (grid:gref matrix-sum i j)
		  (* -1 (learning-rate)))))))

(defun update-matrices (numberofupdates start-index end-index)
  ""
  (loop repeat numberofupdates do
       (elt+ w (change-in-w start-index end-index))
       (elt+ v (change-in-v start-index end-index))))

(defun total-error (start-index end-index)
  ""
  (* 1/2
     (grid:gref (loop for pattern-index from start-index below end-index
		   with matrix-sum = (grid:make-foreign-array 'single-float :dimensions '(1 1) :initial-element 0.0)
		   finally (return matrix-sum) do
		   (elt+ matrix-sum
			 (matrix-product (matrixify (nn-error pattern-index) numberofoutputnodes)
					 (matrix-transpose (matrixify (nn-error pattern-index) numberofoutputnodes)))))
		0 0)))