
(in-package :tableaux)

(defun preproc (wff)
  (let ((trans '((or* . or) (and* . and))))
    (if (and (atom wff) (symbolp wff))
	wff
	(let ((op (if (member (car wff) '(or* and*) :test #'equal)
		      (cdr (assoc (car wff) trans))
		      (car wff)))) 
	  (if (> (length wff) 2) 
	      (reduce (lambda (a b) `(,op ,a ,b)) (mapcar #'preproc (cdr wff)))
	      `(,op ,(preproc (cadr wff))))))))


(defun vestidos* ()
  (let ((theory (append (by-person MA MB MP)
			(by-person CA CB CP)
			(by-person AA AB AP)
			(by-person MA AA CA)
			(by-person MB AB CB)
			(by-person MP AP CP)
			; agora considerando o que cada uma diz:
			(list '(implies AA AB)
			      '(implies CA (not AB))
			      '(not AB)
			      '(implies AP CB)))))
    (cons 'and* theory)))


(defun prove* (wff)
  (prove (preproc wff)))

(defun test-vestidos* ()
  (prove* `(implies ,(vestidos*) (and* MA CB AP))))
