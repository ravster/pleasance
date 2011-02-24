;;;; This code creates the training data set that will be used by the neural-net as input.

(in-package :ravi.nn0)

;; Normalize data
;; Min-Max normalization
;; Score = (max-score - min-score) * ((Data - min-data)/(Max-data - min-data)) + min-score

(defun score (input &key min-output range-output min-input range-input)
  "Function that takes a score and calculates the true-value, or vice-versa."
  (+ min-output
     (* range-output
	(/ (- input min-input)
	   range-input))))

(defun create-scores (bar-array output-array &key (min-output -1) (range-output 2) function-name (index-shift 20) (start-index 0) (end-index 5000) output-array-index)
  "Use the min-max normalization to create scores of input data for the neural network."
  (loop for i below 5000
     with max-input = (loop for j from (+ start-index index-shift) below (+ end-index index-shift)
			 maximize (funcall function-name (aref bar-array j)))
     with min-input = (loop for j from (+ start-index index-shift) below (+ end-index index-shift)
			 minimize (funcall function-name (aref bar-array j)))
     with range-input = (- max-input min-input)
     do
     (setf (aref output-array i output-array-index)
	   (score (funcall function-name (aref bar-array (+ i index-shift))) 
		  :min-output min-output :range-output range-output :min-input min-input :range-input range-input))))

(defmacro create-set (set-name start-index end-index)
  "Create a set of data for the NN"
  `(progn
     (defparameter ,set-name (make-array '(5000 3)))
     (create-scores *array* ,set-name :function-name #'+5close-diff :output-array-index 2 :start-index ,start-index :end-index ,end-index)
     (create-scores *array* ,set-name :function-name #'atrb :output-array-index 1 :start-index ,start-index :end-index ,end-index)
     (create-scores *array* ,set-name :function-name #'ma-diff-close :output-array-index 0 :start-index ,start-index :end-index ,end-index)))

(create-set training-set 0 5000)
(create-set validation-set 5000 10000)
(create-set test-set 10000 15000)

(defun unscore (input raw-data &key function-name (start-index 0) (end-index 5000) (index-shift 20) (min-input -1) (range-input 2))
  "This is the reciprocal of the score-function.  It takes a min-max score, and then finds out what value that score corresponds to in the real (Non-NN) world."
  (let* ((max-output (loop for i from (+ start-index index-shift) below (+ end-index index-shift)
			maximize (funcall function-name (aref raw-data i))))
	 (min-output (loop for i from (+ start-index index-shift) below (+ end-index index-shift)
			minimize (funcall function-name (aref raw-data i))))
	 (range-output (- max-output min-output)))
    (score input :min-output min-output :range-output range-output :min-input min-input :range-input range-input)))
