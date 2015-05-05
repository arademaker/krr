
(in-package :tableaux)

(defun atomic (a b c)
  (intern (format nil "S~a~a~a" a b c)))


(defmacro big ((op var start end) &body body)
  `(cons (quote ,op)
	 (loop for ,var from ,start to ,end
	       collect
	       ,@body)))


(defun clauses-1 ()
  "There is at least one number in each entry."
  (big (and x 1 9)
    (big (and y 1 9)
      (big (or z 1 9)
	(atomic x z y)))))


(defun clauses-2 ()
  "Each number appears at most once in each row."
  (big (and y 1 9)
    (big (and z 1 9)
      (big (and x 1 8)
	(big (and i (1+ x) 9)
	  `(or (not ,(atomic x y z))
	       (not ,(atomic i y z))))))))


(defun clauses-3 ()
  "Each number appears at most once in each column."
  (big (and x 1 9)
    (big (and z 1 9)
      (big (and y 1 8)
	(big (and i (1+ y) 9)
	  `(or (not ,(atomic x y z))
	       (not ,(atomic x i z))))))))


(defun clauses-4 ()
  "Each number appears at most once in each 3x3 sub-grid."
  "...")

(defun tabuleiro ()
  "...")

(defun teste-sudoku ()
  "..."
  (prove* `(and* ,(tabuleiro ,(clauses-3) ,(clauses-2) ,(clauses-1)))))
