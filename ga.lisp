(in-package :ravi.nn0)

(defun make-chromosome (length-of-chromosome)
  "Make a binary chromosome of length 'length-required'."
  (loop repeat length-of-chromosome
     collect (random 2)))

(defun find-fitnesses (chromosome)
  "Take a hash of chromosomes and return their relative fitness-levels in the form of a hash-table"
  (format t "~&Begin find-fitnesses. chromosome is ~A" chromosome)
  (mapcar #'(lambda (k)
	      (setf (second k)
		    (third (nn (first k))))
	      k)
	  chromosome))
;  (format t "~&find-fitnesses: chromosome: ~A" chromosome)

(defun prune-population (size-of-population chromosome)
  "Take a hash-table and remove 1/3 worst chromosomes."
  (assert (not (zerop (length chromosome))) (chromosome) "Prune-population: chromosome list is empty.")
  (format t "~&In prune-population.  Size of population is : ~A" (length chromosome))
  (format t "~&population is: ~A" chromosome)
  ;; Sort the list
  (setf chromosome (sort (copy-list chromosome) #'> :key #'second))
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
;	   (format t "~&inside do-cross, the population is ~A" listorig)
	   (let* ((list (mapcar #'first listorig))
		  (p1 (copy-list  (nth (random (length list)) list)))
		  (p2 (copy-list  (nth (random (length list)) list)))
		  (cross-point (random (length p1)))) ;Since length starts from 1, and #'subseq starts from 0.
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
  (labels ((do-mutate (list-0)
;	     (format t "~&inside do-mutate, the population is ~A" list-0)
	     (let* ((list (mapcar #'first list-0))
		    (p1 (copy-list (nth (random (length list)) list)))
		    (mutate-point (random (length p1)))) ;Since length starts from 1, and we are going to be using this on #'nth, which starts from 0.
	       (if (zerop (nth mutate-point p1))
		   (setf (nth mutate-point p1) 1)
		   (setf (nth mutate-point p1) 0))
	       (if (member p1 list :test #'equal)
		   (do-mutate list-0)
		   p1))))
    (if (>= (length population)
	    size-of-population)
	population
	(let ((new-key (do-mutate (copy-tree population))))
	  (format t "~&recursive-mutation returns ~A" (list (list new-key 'mutation) population))
	  (recursive-mutation size-of-population (push (list new-key 'mutation) population))))))

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
