(in-package :krr-user)

(defun gen-sudoku (n) ;n é o número de elementos iniciais gerados
  (labels ((gen-empty-list ()
	     (let ((res nil))
	       (dotimes (i 81 res) (push 0 res)))))
    (do ((table (gen-empty-list))
	 (line (random 9) (random 9))
	 (column (random 9) (random 9))
	 (number (random 9) (random 9))
	 (k 0 (incf k)))
	((= k n) table)
      (if (good-elt? number line column table)
	  (setf (elt table (+ (* line 9) column)) number)))))

(defun good-elt? (n line column table)
  (let ((flag1 nil) (flag2 nil))
    (not (or (loop for i in (subseq table (* line 9) (* (1+ line) 9))
		 if (= n i) return t)
	      (dotimes (i 9 flag1) (if (= (elt table (+ (* 9 i) column)) n)
				      (setf flag1 t)))
	      (let* ((aux-line (floor (/ line 3))) (aux-column (floor (/ column 3)))
		    (start (+ (* aux-line 9) (* aux-column 3))))
		(dotimes (i 3 flag2)
		  (loop for elt in (subseq table (+ (* 9 i) start) 
					   (+ (* 9 i) start 3)) if (= elt n) return (setf flag2 t)))))))))

(test input-tab
      ;; caso em que há elementos maiores que 9
      (is (equal nil (input-tab
		      '(0 10 8 0 0 0 7 0 0
			0 0 0 3 0 0 2 0 0
			0 7 0 0 0 0 0 0 0
			0 0 0 0 7 1 0 0 0
			6 0 0 0 0 0 0 4 0
			3 0 0 0 0 0 0 0 0
			4 0 0 5 0 0 0 0 3
			0 2 0 0 8 0 0 0 0
			0 0 0 0 0 0 0 6 0))))
      ;; caso em que há mais que 81 elementos 
      (is (equal nil (input-tab
		      '(0 1 8 0 0 0 7 0 0
			0 0 0 3 0 0 2 0 0
			0 7 0 0 0 0 0 0 0
			0 0 0 7 1 0 0 0 6
			0 0 0 0 0 0 4 0 3
			0 0 0 0 0 0 0 0 4
			0 0 5 0 0 0 0 3 0
			2 0 0 8 0 0 0 0 0
			0 0 0 0 0 0 6 0 0 1))))
      ;; caso normal, retorna triplas do tipo (linha coluna valor)
      (is (equal '((1 2 1) (1 3 8) (1 7 7) (2 4 3) (2 7 2) 
		   (3 2 7) (4 5 7) (4 6 1) (5 1 6) (5 8 4)
		   (6 1 3) (7 1 4) (7 4 5) (7 9 3) (8 2 2) (8 5 8) (9 8 6))
                 (input-tab
                  '(0 1 8 0 0 0 7 0 0
		    0 0 0 3 0 0 2 0 0
		    0 7 0 0 0 0 0 0 0
		    0 0 0 0 7 1 0 0 0
		    6 0 0 0 0 0 0 4 0
		    3 0 0 0 0 0 0 0 0
		    4 0 0 5 0 0 0 0 3
		    0 2 0 0 8 0 0 0 0
		    0 0 0 0 0 0 0 6 0)))))
