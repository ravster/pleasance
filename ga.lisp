(in-package :ravi.nn0)

(defun make-chromosome (length-of-chromosome)
  "Make a binary chromosome of length 'length-required'."
  (loop repeat length-of-chromosome
     collect (random 2)))

(defun find-fitnesses (chromosome-hash)
  "Take a hash of chromosomes and return their relative fitness-levels in the form of a hash-table"
  (format t "~&Begin find-fitnesses")
  (loop for k being the hash-key in chromosome-hash do
       (setf (gethash k chromosome-hash)
	     (first (nn k)))
       finally (return chromosome-hash)))

(defun prune-population (size-of-population hash-of-population)
  "Take a hash-table and remove 1/3 worst chromosomes."
  (assert (not (zerop (hash-table-count hash-of-population))) (hash-of-population) "Function Prune-population: Hash table empty.")
  (format t "~&In prune population. Size of hash-table is ~A" (hash-table-count hash-of-population))
  (loop while (> (hash-table-count hash-of-population)
		 (* size-of-population
		    2/3))
     with maximum-error = 0 do
       (format t "~&in prune-population loop")
       (setf maximum-error (loop for v being the hash-value in hash-of-population maximize v))
       (maphash #'(lambda (k v)
		    (if (= v maximum-error)
			(remhash k hash-of-population)))
		hash-of-population)
     finally (return hash-of-population)))

(defun crossover (length-of-chromosome size-of-population hash-of-population)
  "Take a pruned hash-table.  Effect crossover.  The resulting hash does not have to be full-sized."
  (format t "~&Size of pruned population : ~A" (hash-table-count hash-of-population))
  (let ((vector-of-parents (make-array (hash-table-count hash-of-population)
				       :initial-contents (loop for k being the hash-keys in hash-of-population
							    collect k)))) ;Easy to access vector of keys
    (loop repeat 20			;Should find some way of making this not arbitrary, but also leave some space for mutations.
       for i = (random (length vector-of-parents)) then (random (length vector-of-parents))
       and j = (random (length vector-of-parents)) then (random (length vector-of-parents))
       and crossover-point = (random length-of-chromosome) then (random length-of-chromosome)
       with first-parent = () and second-parent = () 
       while (< (hash-table-count hash-of-population)
		 size-of-population)
       finally (let ((return-list (loop for k being the hash-keys in hash-of-population collect k)))
		 (format t "~&Crossover returns list: ~A" return-list)
		 (return hash-of-population))
       do
	 (setf first-parent (aref vector-of-parents i)
	       second-parent (aref vector-of-parents j)) ;Pick 2 to make parents off
	 (rotatef (subseq first-parent crossover-point) (subseq second-parent crossover-point))
	 (setf (gethash first-parent hash-of-population) 1
	       (gethash second-parent hash-of-population) 1)
	 )))

(defun recursive-mutation (size-of-population chromosome-hash)
  ""
  (check-type chromosome-hash hash-table)
  (assert (< 0 (hash-table-count chromosome-hash)) (chromosome-hash) "Error in argument to #'recursive-mutation.  Hash-table is empty.")
  (format t "~&In recursive mutation")
  (flet ((mutate-chromosome (chromosome)
	   (check-type chromosome list "Within mutate-chromosome within recursive-mutation.  Argument is not a list.")
	   (let ((i (random (length chromosome))))
	     (if (zerop (nth i chromosome))
		 (setf (nth i chromosome) 1)
		 (setf (nth i chromosome) 0)))))
    (if (>= (hash-table-count chromosome-hash)
	    size-of-population)
	chromosome-hash	  ;Return the new hash as the next generation.
	(let* ((list-of-chromosomes (loop for k being the hash-keys in chromosome-hash collect k))
	       (i (nth (random (length list-of-chromosomes))
		       list-of-chromosomes))) ;Pick a random chromosome.
	  (setf (gethash (mutate-chromosome i) chromosome-hash)
		nil) ;Place a mutated version of it in the hash-table.
	  (recursive-mutation size-of-population chromosome-hash)))))

(defun ga (length-of-chromosome size-of-population number-of-generations)
  "Run the GA through a given number of generations and return a list of chromosomes that made the final cut"
    (loop for generation-iterator below number-of-generations
       with chromosome-hash = (make-hash-table :test 'equal)
       initially (loop while (< (hash-table-count chromosome-hash)
       			     size-of-population) do
       		    (format t "~&making first hash-table")
       		    (setf (gethash (make-chromosome length-of-chromosome) chromosome-hash) 0) ;Create a new hash-entry.
       		    finally (format t "~&End original hash loop"))
       do
       (format t "~&Begin generation: ~A" generation-iterator)
       (setf chromosome-hash (recursive-mutation size-of-population
						 (crossover length-of-chromosome size-of-population
							    (prune-population size-of-population
									      (find-fitnesses chromosome-hash))))) ;Update the list of chromosomes after selection and crossover.
       finally (return chromosome-hash)))
