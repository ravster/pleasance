;; Copyright 2011-2012 Ravi Desai <rd7190@gmail.com>
;; Distributed under the terms of the GNU Affero GPL version 3 or any later version.

(in-package :pleasance)

(defun make-chromosome (length-of-chromosome)
  "Make a binary chromosome of length 'length-required'."
  (loop repeat length-of-chromosome
     collect (random 2)))

(defun print-hash (hash-table)
    (let ((foo ""))
      (maphash #'(lambda (k v)
		   (setf foo (concatenate 'string foo (format nil "~%~A : ~A" k v))))
	       hash-table)
      foo))

(defun find-fitnesses (population)
  "Take a hash of chromosomes and return their relative fitness-levels in the form of a hash-table"
  (format t "~&Begin find-fitnesses. population is ~A" population)
  (loop for k being the hash-key in population do
       (setf (gethash k population) (third (nn k))))
  population)

(defun prune-population (size-of-population population)
  "Take a hash-table and reduce it to size-of-population by removing the worst scores."
  ;; Algorithm:
  ;; Extract v; sort v; put all k and v from the biggest v into a new hash-table (In descending order of v); return new h-table
  (format t "~&In prune-population.  Size of population is : ~A" (hash-table-count population))
  (format t "~&population is: ~A" (print-hash population))
  (let ((list-of-scores))
    ;; Put scores in list
    (loop for value being the hash-values in population do
	 (push value list-of-scores))
    ;; sort list-of-scores in descending order.
    (setf list-of-scores (sort list-of-scores #'>))
    ;; Return all k & v that match the first 'size-of-population' v's in the sorted list
    (loop repeat size-of-population
       for score in list-of-scores
       with new-hash-table = (make-hash-table :test 'equal) do
	 (maphash #'(lambda (k v)
		      (when (= v score)
			(setf (gethash k new-hash-table) v)))
		  population)
	 finally (return new-hash-table))))	 

(defun crossover (size-of-population population)
  "Take a pruned hash-table.  Effect crossover."
  ;; Algorithm:
  ;; make list of all k where the v is numeric; do crossover of those k; put children into hash-table; repeat (recursion);  return h-table that is bigger.  It will be pruned;
  (format t "~&start crossover: population size: ~a~A" (hash-table-count population) (print-hash population))
  (flet ((do-cross (original-keys)
	   ;; Algorithm:
	   ;; Get a list of keys; identify 2 random keys; identify a cross-point; do the crossover; return the 2 new keys in a list
	   (let ((origin-key-1 (nth (random (length original-keys))
				    original-keys))
		 (origin-key-2 (nth (random (length original-keys))
				    original-keys))
		 (cross-point (random (length (first original-keys)))))
	     (rotatef (subseq origin-key-1 cross-point) (subseq origin-key-2 cross-point))
	     (list origin-key-1 origin-key-2))))
    (let ((original-keys))
      (maphash #'(lambda (k v) ;Make list of all k where the v is numeric.
		   (if (numberp v)
		       (push (copy-list k) original-keys)))
	       population)
      ;; Put crossover children into new hash-table and return it
      (loop while (< (hash-table-count population)
		     (* 4/3 size-of-population)) ;Build the hash-table by 1/3 of its target size.
	 do
	 (mapcar #'(lambda (new-key)
		     ;; If the key does not exist, put it in the hash-table, else leave it alone.
		     (unless (gethash new-key population)
		       (setf (gethash new-key population) 'crossover)))
		 (do-cross (copy-tree original-keys)))
	   finally (return population)))))

(defun mutation (population)
  ;; Algorithm:  select chromosomes from hash-table; mutate them; insert mutations into hash-table;  Return a h-table that is bigger.  It will be pruned.
  (format t "~&Starting mutation.  population is ~a" (print-hash population))
  (flet ((do-mutate (chromosome)
	   ;; Algorithm:
	   ;; Take list of bits; Randomly select a bit; Flip it (1 to 0 or vice-versa); Return new list
	   (let ((bit-number (random (length chromosome)))) ;The index of the bit that will be flipped.
	     (if (zerop (nth bit-number chromosome))
		 (setf (nth bit-number chromosome) 1) ;If 0, set to 1.
		 (setf (nth bit-number chromosome) 0)) ;If 1, set to 0.
	     chromosome)))	       ;Return the mutated chromosome.
    (let ((origin-chromosomes nil))
      (maphash #'(lambda (k v) ;Only use chromosomes that have survived one generation.
		   (if (numberp v)
		       (push k origin-chromosomes)))
	       population)
      ;; Mutate chromosomes and enter the unique ones into the hash-table.
      (loop while (> 3
		     (loop for v being the hash-values in population
			  if (eql 'mutation v) count v))
	   initially (format t "~&origin-chromosomes : ~a" origin-chromosomes)
	 for new-key = (do-mutate (copy-list (nth (random (length origin-chromosomes))
						  origin-chromosomes))) ;Mutate a random chromosome that has survived 1 generation.
	 then (do-mutate (copy-list (nth (random (length origin-chromosomes))
					 origin-chromosomes)))
	 do
	 (unless (gethash new-key population) ;If the mutation doesn't already exist in the population, then add it.
	   (setf (gethash new-key population) 'mutation)))
      ;; Return the new hash-table.
      (format t "~&End mutation: population is ~a" (print-hash population))
      population)))

(defun ga (length-of-chromosome size-of-population number-of-generations)
  "Run the GA through a given number of generations and return a list of chromosomes that made the final cut"
  (loop for generation-iterator below number-of-generations
     with population = (make-hash-table :test 'equal)
     ;; Initial population.
     initially (loop while (< (hash-table-count population)
			      size-of-population)
		  do
		  (setf (gethash (make-chromosome length-of-chromosome) population)
			t))
     do
     (format t "~&Begin generation: ~A" generation-iterator)
     (setf population (mutation (crossover size-of-population
					   (prune-population size-of-population
							     (find-fitnesses population))))) ;Update the list of chromosomes after selection and crossover.
     finally (return population)))
