
(in-package :tableaux)

(defun atomic (a b c)
  (intern (format nil "S~a~a~a" a b c)))


(defun clauses-1 ()
  "There is at least one number in each entry"
  (cons 'and*
	(loop for x from 1 to 9
	      collect 
	      (cons 'and*
		    (loop for y from 1 to 9
			  collect 
			  (cons 'or*
				(loop for z from 1 to 9
				      collect
				      (atomic x y z))))))))


(defun clauses-2 ()
  "Each number appears at most once in each row"
  (cons 'and* 
	(loop for y from 1 to 9
	      collect
	      (cons 'and*
		    (loop for z from 1 to 9
			  collect 
			  (cons 'and*
				(loop for x from 1 to 8
				      collect 
				      (cons 'and*
					    (loop for i from (1+ x) to 9
						  collect
						  `(or (not ,(atomic x y z))
						       (not ,(atomic i y z))))))))))))

(defun clauses-3 ()
  "Each number appears at most once in each column"
  (cons 'and*
	(loop for y from 1 to 9
	      collect
	      (cons 'and*
		    (loop for z from 1 to 9
			  collect 
			  (cons 'and*
				(loop for x from 1 to 8
				      collect 
				      (cons 'and* 
					    (loop for i from (1+ x) to 9
						  collect
						  `(or (not ,(atomic x y z))
						       (not ,(atomic x i z))))))))))))


(defun clauses-4 ()
  "Each number appears at most once in each 3x3 sub-grid"
  "...")


;; testando
;; (prove* `(and* ,(clauses-3) ,(clauses-2) ,(clauses-1)))

