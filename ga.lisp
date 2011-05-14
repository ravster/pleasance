(in-package :ravi.nn0)

(defun make-chromosome (length-of-chromosome)
  "Make a binary chromosome of length 'length-required'."
  (loop repeat length-of-chromosome
       collect (random 2)))

(defun find-fitnesses (length-of-chromosome size-of-population)
  "Return a hash-table of 'size-of-population' chromosomes along with their corresponding fitness results"
  (loop repeat size-of-population
     with population = (make-hash-table :size size-of-population)
     and chromosome = () do
     (setf chromosome (make-chromosome length-of-chromosome))
     (setf (gethash chromosome population) (nn chromosome))
     finally (return population)))

(defun find-better-half-from-population (length-of-chromosome size-of-population)
  (let* ((hash-of-population (find-fitnesses length-of-chromosome size-of-population))
	 (average-score (/ (loop for v being the hash-value in hash-of-population
			      sum (car v)) ;Since 'v' is actually a list returned by 'nn-for-ga'
			   size-of-population))) ;To get the average score that is returned
    (loop for v being the hash-value in hash-of-population using (hash-key k) do
	 (when (> (car v)
		  average-score)
	   (remhash k hash-of-population))
	 finally (return hash-of-population))))
