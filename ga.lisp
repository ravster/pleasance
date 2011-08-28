(in-package :ravi.nn0)

(defun make-chromosome (length-of-chromosome)
  "Make a binary chromosome of length 'length-required'."
  (loop repeat length-of-chromosome
     collect (random 2)))

(defun find-fitnesses (chromosome)
  "Take a hash of chromosomes and return their relative fitness-levels in the form of a hash-table"
  (format t "~&Begin find-fitnesses")
  (mapc #'(lambda (k)
	    (setf (second k)
		  (first (nn (first k)))))
	chromosome)
  chromosome)

(defun prune-population (size-of-population chromosome)
  "Take a hash-table and remove 1/3 worst chromosomes."
  (assert (not (zerop (length chromosome))) (chromosome) "Prune-population: chromosome list is empty.")
  (format t "~&In prune-population.  Size of population is : ~A" (length chromosome))
  (format t "~&population is: ~A" chromosome)
  ;; Sort the list
  (setf chromosome (sort (copy-list chromosome) #'< :key #'second))
  (format t "~&Sorted population is: ~A" chromosome)
  ;; Prune the bottom third of the vector
  (loop repeat (* size-of-population 2/3)
     for i in chromosome
     collect i into pruned-list
     finally (progn
	       (format t "~&Pruned list: ~A" pruned-list)
	       (return pruned-list))))

(defun crossover (length-of-chromosome size-of-population population)
  "Take a pruned hash-table.  Effect crossover.  The resulting hash does not have to be full-sized."
  (labels ((do-cross (listorig)
	   (format t "~&inside do-cross, the population is ~A" listorig)
	   (let* ((list (mapcar #'first listorig))
		  (p1 (copy-list  (nth (random (length list)) list)))
		  (p2 (copy-list  (nth (random (length list)) list)))
		  (cross-point (random (length p1))))
	     (rotatef (subseq p1 cross-point) (subseq p2 cross-point))
;	     (format t "~&p1= ~A , p2 = ~A" p1 p2)
	     (if (member p1 list :test #'equal)
		 (if (member p2 list :test #'equal)
		     (do-cross (copy-tree listorig))
		     p2)
		 p1))))
    (if (>= (1+ (length population))
	    size-of-population)
	;; End crossover and return the filled population.
	population
	;; Create a new key and call crossover again with the original key plus the new key.
	(let ((new-key (do-cross (copy-tree population))))
	  (crossover length-of-chromosome size-of-population (push (list new-key 'crossover) population))))))

(defun recursive-mutation (size-of-population population)
  ""
  (format t "~&in recursive-mutation. population of size ~A is : ~A" (length population) population)
  (flet ((mutate-chromosome (chromosome)
	   (let ((i (random (length chromosome))))
	     (if (zerop (nth i chromosome))
		 (setf (nth i chromosome) 1)
		 (setf (nth i chromosome) 0)))))
    (let ((pop-copy (mapcar #'first population)))
      (loop while (< (length population)
		     size-of-population)
	 with mutation = ()
	 for i = (nth (random (length pop-copy)) pop-copy) then (nth (random (length pop-copy)) pop-copy)
	 do
	 (setf mutation (mutate-chromosome i))
	 (unless (member mutation population :test #'equal)
	   (push (list mutation 'mutation) population))
	 finally (return population)))))

  ;; (check-type chromosome-hash hash-table)
  ;; (assert (< 0 (hash-table-count chromosome-hash)) (chromosome-hash) "Error in argument to #'recursive-mutation.  Hash-table is empty.")
  ;; (format t "~&In recursive mutation")
  ;; (flet ((mutate-chromosome (chromosome)
  ;; 	   (check-type chromosome list "Within mutate-chromosome within recursive-mutation.  Argument is not a list.")
  ;; 	   (let ((i (random (length chromosome))))
  ;; 	     (if (zerop (nth i chromosome))
  ;; 		 (setf (nth i chromosome) 1)
  ;; 		 (setf (nth i chromosome) 0)))))
  ;;   (if (>= (hash-table-count chromosome-hash)
  ;; 	    size-of-population)
  ;; 	chromosome-hash	  ;Return the new hash as the next generation.
  ;; 	(let* ((list-of-chromosomes (copy-list (loop for k being the hash-keys in chromosome-hash collect k)))
  ;; 	       (i (nth (random (length list-of-chromosomes))
  ;; 		       list-of-chromosomes))) ;Pick a random chromosome.
  ;; 	  (setf (gethash (mutate-chromosome i) chromosome-hash)
  ;; 		nil) ;Place a mutated version of it in the hash-table.
  ;; 	  (recursive-mutation size-of-population chromosome-hash)))))

(defun ga (length-of-chromosome size-of-population number-of-generations)
  "Run the GA through a given number of generations and return a list of chromosomes that made the final cut"
    (loop for generation-iterator below number-of-generations
       with chromosome = ()
       initially (loop while (< (length chromosome)
				size-of-population)
		    with foo = () do
		      (setf foo (list (make-chromosome length-of-chromosome) t))
		      (unless (find (first foo) chromosome :test 'equal :key #'first)
			(push foo chromosome)))
       do
       (format t "~&Begin generation: ~A" generation-iterator)
       (setf chromosome (recursive-mutation size-of-population
						 (crossover length-of-chromosome size-of-population
							    (prune-population size-of-population
									      (find-fitnesses chromosome))))) ;Update the list of chromosomes after selection and crossover.
       finally (return chromosome)))
