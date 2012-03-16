;; Copyright 2011-2012 Ravi Desai <rd7190@gmail.com>
;; Distributed under the terms of the GNU Affero GPL version 3 or any later version.

;;;; This code creates the training data set that will be used by the neural-net as input.

(in-package :pleasance)

;; Normalize data
;; Min-Max normalization
;; Score = (max-score - min-score) * ((Data - min-data)/(Max-data - min-data)) + min-score

(defun score (input &key min-output range-output min-input range-input)
  "Function that takes a score and calculates the true-value, or vice-versa."
  (+ min-output
     (* range-output
	(/ (- input min-input)
	   range-input))))

(defun unscore (input raw-data &key function-name (start-index 0) (end-index 1000) (index-shift 50) (min-input -1) (range-input 2))
  "This is the reciprocal of the score-function.  It takes a min-max score, and then finds out what value that score corresponds to in the real (Non-NN) world."
  (let* ((max-output (loop for i from (+ start-index index-shift) below (+ end-index index-shift)
			maximize (funcall function-name (aref raw-data i))))
	 (min-output (loop for i from (+ start-index index-shift) below (+ end-index index-shift)
			minimize (funcall function-name (aref raw-data i))))
	 (range-output (- max-output min-output)))
    (score input :min-output min-output :range-output range-output :min-input min-input :range-input range-input)))

(defun create-scores (bar-array output-array &key (min-output -1) (range-output 2) function-name (index-shift 50) (start-index 0) (end-index 5000) output-array-index)
  "Use the min-max normalization to create scores of input data for the neural network."
  (loop for i below (- end-index start-index)
     with max-input = (loop for j from (+ start-index index-shift) below (+ end-index index-shift)
			 maximize (funcall function-name (aref bar-array j)))
     with min-input = (loop for j from (+ start-index index-shift) below (+ end-index index-shift)
			 minimize (funcall function-name (aref bar-array j)))
     with range-input = (- max-input min-input)
     do
     (setf (aref output-array i output-array-index)
	   (score (funcall function-name (aref bar-array (+ i index-shift))) 
		  :min-output min-output :range-output range-output :min-input min-input :range-input range-input))))

(defparameter *training-functions* (list #'+5close-diff #'atrb #'ma-diff-close #'adx #'so #'mso #'sso #'roc #'momentum #'movar #'disparity-5 #'disparity-10 #'price-oscillator))

(defun create-set (*data-vector* *set-name* start-index end-index *list-of-functions*)
  "This function will create the datasets that hold the scored (Normalized) data that will be used by the NNs."
  (loop for function-name in *list-of-functions*
       for function-count from 0 do
       (create-scores *data-vector* *set-name* :function-name function-name :output-array-index function-count :start-index start-index :end-index end-index)))

;;; The following defparams are for setting up the datasets of the normalized data.  These names presently have to be used since the nn-function is hard-coded to use them.  And I don't care to change that since re-calculating these things takes practically no time.
(defparameter training-set nil)
(defparameter validation-set nil)
(defparameter test-set nil)

(defun process-data (raw-data data-vector)
  ;; Populate the bar-array.
  (read-ohlc raw-data data-vector)
  ;; Start calculating indicators and placing their values in the bar-array.
  (calculate-true-range data-vector)
  (calculate-atr data-vector 20)
  (moving-average data-vector 20)
  (ma-difference-from-close data-vector 20)
  (+5close-present data-vector)
  (calc-dm data-vector)
  (calc-di data-vector)
  (calc-avg-di data-vector 14)
  (calc-dmi data-vector 14)
  (calc-adx data-vector 14)
  (stochastic-oscillator data-vector 20)
  (moving-stochastic-oscillator data-vector 20)
  (slow-stochastic-oscillator data-vector 20)
  (rate-of-change data-vector 10)
  (calc-momentum data-vector 10)
  (moving-variance data-vector 20)
  (disparity-5-calc data-vector)
  (disparity-10-calc data-vector)
  (price-oscillator-calc data-vector)
  ;; Done calculating the indicators.

  ;; Start scaling the indicators for the NN.
  (create-set data-vector training-set 0 1000 *training-functions*)
  (create-set data-vector validation-set 1500 1900 *training-functions*)
  (create-set data-vector test-set 1000 1400 *training-functions*))
