
(in-package :krr-user)


(defun atomic (a b c)
  (intern (format nil "S~a~a~a" a b c)))


(defmacro big ((op var start end) &body body)
  `(cons (quote ,op)
    	 (loop for ,var from ,start to ,end
	       collect
	       ,@body)))


(defun clauses-1 ()
  "There is at least one number in each entry"
  (big (and x 1 9)
    (big (and y 1 9)
      (big (or z 1 9) (atomic x z y)))))


(defun clauses-2 ()
  "Each number appears at most once in each row"
  (big (and y 1 9)
    (big (and z 1 9)
      (big (and x 1 8)
	(big (and i (+ x 1) 9)
	  `(or (not ,(atomic x y z))
	       (not ,(atomic i y z))))))))


(defun clauses-2b ()
  "Each number appears at most once in each row"
  (big (and y 1 9)
    (big (and z 1 9)
      (big (and x 1 8)
	`(implies ,(atomic x y z)
		  (not ,(big (or i (+ x 1) 9)
			     (atomic i y z))))))))


(defun clauses-3 ()
  "Each number appears at most once in each column"
  (big (and x 1 9)
    (big (and z 1 9)
      (big (and y 1 8)
	(big (and i (+ y 1) 9)
	  `(or (not ,(atomic x y z))
	       (not ,(atomic x i z))))))))


(defun clauses-3b ()
  "Each number appears at most once in each column"
  (big (and x 1 9)
    (big (and z 1 9)
      (big (and y 1 8)
	`(implies ,(atomic x y z)
		  (not ,(big (or i (+ y 1) 9)
			     (atomic x i z))))))))


(defun clauses-4 ()
  "Each number appears at most once in each 3x3 sub-grid"
  `(and
    ,(big (and z 1 9)
      (big (and i 0 2)
        (big (and j 0 2)
	  (big (and x 1 3)
	    (big (and y 1 2)
	      (big (and k (+ y 1) 3)
		`(or (not ,(atomic (+ x (* 3 i)) (+ y (* 3 j)) z))
		     (not ,(atomic (+ x (* 3 i)) (+ k (* 3 j)) z)))))))))
    ,(big (and z 1 9)
      (big (and i 0 2)
        (big (and j 0 2)
	  (big (and y 1 3)
	    (big (and x 1 2)
	      (big (and k (+ x 1) 3)
	        (big (and l 1 3)
		  `(or (not ,(atomic (+ x (* 3 i)) (+ y (* 3 j)) z))
		       (not ,(atomic (+ k (* 3 i)) (+ l (* 3 j)) z))))))))))))


(defun clauses-4b ()
  "Each number appears at most once in each 3x3 sub-grid"
  `(and
    ,(big (and z 1 9)
      (big (and i 0 2)
        (big (and j 0 2)
	  (big (and x 1 3)
	    (big (and y 1 2)
	      `(implies ,(atomic (+ (* 3 i) x) (+ (* 3 j) y) z)
			(not ,(big (or k (+ y 1) 3)
				   (atomic (+ (* 3 i) x)
					   (+ (* 3 j) k) z)))))))))
    ,(big (and z 1 9)
      (big (and i 0 2)
        (big (and j 0 2)
	  (big (and y 1 3)
	    (big (and x 1 2)
	      (big (and l 1 3)
		`(implies ,(atomic (+ (* 3 i) x) (+ (* 3 j) y) z)
			 (not ,(big (or k (+ x 1) 3)
				    (atomic (+ (* 3 i) k) (+ (* 3 j) k) z))))))))))))


(defun clauses-5 ()
  "There is at most one number in each entry"
  (big (and x 1 9)
    (big (and y 1 9)
      (big (and z 1 8)
	(big (and i (+ z 1) 9)
	  `(or (not ,(atomic x y z))
	       (not ,(atomic x y i))))))))


(defun clauses-5b ()
  "There is at most one number in each entry"
  (big (and x 1 9)
    (big (and y 1 9)
      (big (and z 1 8)
	`(implies ,(atomic x y z)
		  (not ,(big (or i (+ z 1) 9)
			     (atomic x y i))))))))


(defun clauses-6 ()
  "Each number appears at least once in each row"
  (big (and y 1 9)
    (big (and z 1 9)
      (big (or x 1 9) (atomic x y z)))))


(defun clauses-7 ()
  "Each number appears at least once in each column"
  (big (and x 1 9)
    (big (and z 1 9)
      (big (or y 1 9) (atomic x y z)))))


(defun clauses-8 ()
  "Each number appears at least once in each 3×3 sub-grid"
  (big (or z 1 9)
    (big (and i 0 2)
      (big (and j 0 2)
	(big (and x 1 3)
	  (big (and y 1 3) (atomic (+ x (* 3 i)) (+ y (* 3 j)) z)))))))



(defun all-clauses ()
  `(and ,(clauses-1) ,(clauses-2) ,(clauses-3) ,(clauses-4)
	,(clauses-5) ,(clauses-6) ,(clauses-7) ,(clauses-8)))



(defun input-tab (tab-list)
  (do ((tabuleiro (mapcar #'(lambda (i)
			      (subseq tab-list i (+ 9 i))) '(0 9 18 27 36 45 54 63 72)))
       (line 0 (+ line 1))
       (elements nil))
      ((= line 9) (reverse elements))
    (let ((lista (nth line tabuleiro)))
      (dolist (elt lista elements)
	(if (not (equal elt 0))
	    (push (list (+ line 1) (+ 1 (position elt lista)) elt)
		  elements))))))


(defun solve-sudoku (tab)
  (prove `(not (and ,(all-clauses)
		    ,(cons 'and (mapcar (lambda (i) (atomic (car i) (cadr i) (caddr i)))
					(input-tab tab)))))))


(defun show-sudoku (sentence)
  (let ((res (loop for i from 1 to 81 collect 0)))
    (dolist (a sentence res)
      (let ((b (symbol-name (formula-frm a))))
	(setf (elt res (- (+ (* 9 (parse-integer (subseq b 1 2))) 
			     (parse-integer (subseq b 2 3)))
			  1))
	      (parse-integer (subseq b 2 3)))))))


(defparameter *sudoku-1* '(0 1 8 0 0 0 7 0 0
			   0 0 0 3 0 0 2 0 0
			   0 7 0 0 0 0 0 0 0
			   0 0 0 0 7 1 0 0 0
			   6 0 0 0 0 0 0 4 0
			   3 0 0 0 0 0 0 0 0
			   4 0 0 5 0 0 0 0 3
			   0 2 0 0 8 0 0 0 0
			   0 0 0 0 0 0 0 6 0))
