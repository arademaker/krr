
(in-package :tableaux)

(defun atomic (a b c)
  (intern (format nil "S~a~a~a" a b c)))


(defmacro big ((op var start end) &body body)
  `(cons (quote ,op)
	 (loop for ,var from ,start to ,end
	       collect
	       ,@body)))


(defun clauses-1 ()
  "There is at least one number in each entry"
  (big (and x 1 9) (big (and y 1 9) (big (or z 1 9) (atomic x z y)))))


(defun clauses-2 ()
  "Each number appears at most once in each row"
  (big (and y 1 9)
       (big (and z 1 9)
            (big (and x 1 8)
                 (big (and i (+ x 1) 9)
                      (or (not (atomic x y z))
                          (not (atomic i y z))))))))


(defun clauses-3 ()
  "Each number appears at most once in each column"
  (big (and x 1 9)
       (big (and z 1 9)
            (big (and y 1 8)
                 (big (and i (+ y 1) 9)
                      (or (not (atomic x y z))
                          (not (atomic x i z))))))))


(defun clauses-4 ()
  "Each number appears at most once in each 3x3 sub-grid"
  (and
    (big (and z 1 9)
         (big (and i 0 2)
              (big (and j 0 2)
                   (big (and x 1 3)
                        (big (and y 1 3)
                             (big (and k (+ y 1) 3)
                                  (or (not (atomic (+ x (* 3 i)) (+ y (* 3 j)) z))
                                      (not (atomic (+ x (* 3 i)) (+ k (* 3 j)) z)))))))))
    (big (and z 1 9)
         (big (and i 0 2)
              (big (and j 0 2)
                   (big (and x 1 3)
                        (big (and y 1 3)
                             (big (and k (+ y 1) 3)
                                  (big (and l 1 3)
                                       (or (not (atomic (+ x (* 3 i)) (+ y (* 3 j)) z))
                                           (not (atomic (+ x (* 3 i)) (+ k (* 3 j)) z))))))))))))


(defun clauses-5 ()
  "There is at most one number in each entry"
  (big (and x 1 9) (big (and y 1 9) (big (and z 1 8) (big (and i (+ z 1) 9) (or (not (atomic x y z))
                                                                                (not (atomic x y i))))))))


(defun clauses-6 ()
  "Each number appears at least once in each row"
  (big (and y 1 9) (big (and z 1 9) (big (or x 1 9) (atomic x y z)))))


(defun clauses-7 ()
  "Each number appears at least once in each column"
  (big (and x 1 9) (big (and z 1 9) (big (or y 1 9) (atomic x y z)))))


(defun clauses-8 ()
  "Each number appears at least once in each 3×3 sub-grid"
  (big (and z 1 9) (big (or i 0 2) (big (or j 0 2) (big (or x 1 3) (big (or y 1 3) (atomic (+ x (* 3 i))
                                                                                           (+ y (* 3 j))
                                                                                           z)))))))


(defun tabuleiro ()
  "...")

(defun teste-sudoku ()
  "..."
  (prove* `(and* ,(tabuleiro ,(clauses-3) ,(clauses-2) ,(clauses-1)))))
