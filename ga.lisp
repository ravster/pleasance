(in-package :ravi.nn0)

(defun make-chromosome (length-of-chromosome)
  "Make a binary chromosome of length 'length-required'."
  (loop repeat length-of-chromosome
       collect (random 2)))

(defun find-fitnesses (list-of-chromosomes)
  "Take a list of chromosomes and return their relative fitness-levels in the form of a hash-table"
  (loop repeat (length list-of-chromosomes)
     with population = (make-hash-table :size (length list-of-chromosomes))
     and chromosome = () do
     (setf chromosome (pop list-of-chromosomes) ;Random chromosome
	   (gethash chromosome population) (nn chromosome))
     finally (return population)))

(defun find-better-half-from-population (hash-of-population)
  "Take a hash-table and remove all entries where the score is higher than the average."
  (let* ((average-score (/ (loop for v being the hash-value in hash-of-population
			      sum (car v)) ;Since 'v' is actually a list returned by 'nn-for-ga'
			   (hash-table-count hash-of-population)))) ;To get the average score that is returned
    (loop for v being the hash-value in hash-of-population using (hash-key k) do
	 (when (> (car v)
		  average-score)
	   (remhash k hash-of-population))
	 finally (return hash-of-population))))

(defun crossover (length-of-chromosome size-of-population hash-of-population)
  "Take a pruned hash-table.  Effect crossover.  Return a full-sized list of chromosomes."
  (let ((list-of-hash-keys (loop for k being the hash-keys in hash-of-population
			     collect k))) ;Easy to access list of keys
;	(size-of-population (hash-table-count hash-of-population)))
    (loop repeat size-of-population
	 with first-parent = () and second-parent = () and temporary-list = ()
	 and number-of-parents = (hash-table-count hash-of-population)
	 and crossover-point = 0
	 and list-of-children = ()
	 until (> (+ (length list-of-hash-keys) (length list-of-children))
		  size-of-population)
	 finally (return (append list-of-hash-keys list-of-children)) ;Return a list of chromosomes for the next round of testing.  There will be 'size-of-population' chromosomes here.
	 do
	 (setf first-parent (nth (random number-of-parents) list-of-hash-keys)
	       second-parent (nth (random number-of-parents) list-of-hash-keys) ;Pick 2 to make parents off
	       crossover-point (random length-of-chromosome)
	       temporary-list (subseq first-parent crossover-point) ;Switch subsequences between the lists to make 2 new children.
	       (subseq first-parent crossover-point) (subseq second-parent crossover-point)
	       (subseq second-parent crossover-point) temporary-list)
	 (pushnew first-parent list-of-children)
	 (pushnew second-parent list-of-children))))

(defun ga (length-of-chromosome size-of-population number-of-generations)
  "Run the GA through a given number of generations and return a list of chromosomes that made the final cut"
  (loop repeat number-of-generations
       with list-of-chromosomes = (loop repeat size-of-population collect (make-chromosome length-of-chromosome)) ;Make the initial random chromosomes
       do
       (setf list-of-chromosomes (crossover length-of-chromosome size-of-population
					    (find-better-half-from-population (find-fitnesses list-of-chromosomes)))) ;Update the list of chromosomes after selection and crossover.
       finally (return list-of-chromosomes)))


; Function to effect mutation
