;; School stuff
;;
;; 2016 :: Erik Granholm


;; total iterations
(defparameter *T* 1000)
;; tabu memory
(defparameter *M* 10)
;; total knapsack capacity
(defparameter *C* 750)
;; penalty multiplier
(defparameter *P* 30)
;; item values
(defparameter *V* '(135 139 149 150 156 163 173 184 192 201 210 214 221 229 240))
;; item weights
(defparameter *W* '(70 73 77 80 82 87 90 94 98 106 110 113 115 118 120))
;; recently moved items
(defparameter *tabu-list* nil)


(defstruct (solution
	(:constructor make-solution (&key values)))
	(values nil :type list))

(defstruct (tabu-entry
	(:constructor make-tabu-entry (&key index iteration)))
	(iteration 0 :type integer)
	(index 0 :type integer))

(defstruct (pair
	(:constructor make-pair (&key left right)))
	(left -1 :type integer)
	(right -1 :type integer))


;; The global solution
(defparameter *solution* (make-solution :values '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(defun calculate-solution-value (solution)
	(reduce #'+ (mapcar #'* (solution-values solution) *V*)))

(defun calculate-solution-weight (solution)
	(reduce #'+ (mapcar #'* (solution-values solution) *W*)))

(defun calculate-solution-fitness (solution)
	(- (calculate-solution-value solution)
		(* *P* (max 0 (- (calculate-solution-value solution) (reduce #'+ *W*))))))

(defun add-tabu (idx iteration)
	;(format t "Adding tabu: iteration: ~d idx: ~d~%" iteration idx)
	(setf *tabu-list* (append *tabu-list* (list (make-tabu-entry :index idx :iteration iteration)))))

(defun free-tabu (iteration)
	;(format t "iteration: ~d  ::  tabu list length: ~d ~%" iteration (list-length *tabu-list*))
	(let ((filtered nil))
		(loop for entry in *tabu-list* do
			;(format t "entry-iteration: ~d   ::  diff: ~d ~%" (tabu-entry-iteration entry) (- iteration (tabu-entry-iteration entry)))
			(if (not (= iteration (tabu-entry-iteration entry)))
				(setf filtered (append filtered (list entry)))))
		(setf *tabu-list* filtered)))

(defun is-tabu (idx)
	(loop for entry in *tabu-list* do
		(if (= idx (tabu-entry-index entry))
			(return-from is-tabu T)))
	(return-from is-tabu nil))

(defun flip-nth-in-place (n solution)
	(setf (nth n (solution-values solution)) (if (= 1 (nth n (solution-values solution)))
		(eval 0) (eval 1))))

(defun copy-solution (solution)
	(let ((copy nil))
		(setf copy (make-solution :values (copy-list (solution-values solution))))
		(return-from copy-solution copy)))

(defun possible-adds ()
		(let ((out nil))
			(loop for idx from 0 to (- (list-length *V*) 1) do
				(if (and (not (is-tabu idx)) (= 0 (nth idx (solution-values *solution*))))
					(setf out (append out (list idx)))))
			(return-from possible-adds out)))

(defun possible-rems ()
		(let ((out nil))
			(loop for idx from 0 to (- (list-length *V*) 1) do
				(if (and (not (is-tabu idx)) (= 1 (nth idx (solution-values *solution*))))
					(setf out (append out (list idx)))))
			(return-from possible-rems out)))

;; inefficient, does not eliminate duplicates
(defun possible-swaps ()
	(let ((out nil))
		(loop for i from 0 to (- (list-length *V*) 1) do
			(loop for j from 0 to (- (list-length *V*) 1) do
				(if (and (not (= i j)) (not (= (nth i (solution-values *solution*)) (nth j (solution-values *solution*)))))
					(if (and (not (is-tabu i)) (not (is-tabu j)))
						(setf out (append out (list (make-pair :left i :right j))))))))
		(return-from possible-swaps out)))

(defun update-if-better (idx iteration)
	;(format t "update if better~%")
	(let ((copy (copy-solution *solution*)))
		(flip-nth-in-place idx copy)
		(if (> (calculate-solution-fitness copy) (calculate-solution-fitness *solution*))
			(progn 
				(flip-nth-in-place idx *solution*)
				(add-tabu idx iteration)))))

(defun swap-if-better (pair iteration)
	;(format t "swap if better~%")
	(let ((copy (copy-solution *solution*)))
		(flip-nth-in-place (pair-left pair) copy)
		(flip-nth-in-place (pair-right pair) copy)
		(if (> (calculate-solution-fitness copy) (calculate-solution-fitness *solution*))
			(progn 
				(flip-nth-in-place (pair-left pair) *solution*)
				(flip-nth-in-place (pair-right pair) *solution*)
				(add-tabu (pair-left pair) iteration)
				(add-tabu (pair-right pair) iteration)))))

(loop for iteration from 0 to *T* do
	;; free tabu elements added M iterations ago
	(free-tabu (- iteration *M*))
	;; remove items from knapsack if we profit from it
	(loop for remove-idx in (append (possible-rems) (possible-adds)) do
		(update-if-better remove-idx iteration))
	;; swap items from/to the knapsack if we profit from it
	(loop for swap in (possible-swaps) do
		(swap-if-better swap iteration))	)
	;; add items to knapsack if we prfit from it
	;(loop for add-idx in (possible-adds) do
	;	(update-if-better add-idx iteration)))
	
*solution*
(calculate-solution-value *solution*)
(calculate-solution-weight *solution*)
(calculate-solution-fitness *solution*)
