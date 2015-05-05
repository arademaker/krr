
(in-package :tableaux)


(defun wff? (wff)
  (let ((ops '(and or implies not))) 
    (cond 
      ((and wff (atom wff) (symbolp wff)) t)
      ((and (listp wff)
	    (member (car wff) ops :test #'equal)
	    (wff? (cadr wff))
	    (wff? (caddr wff))) t) 
      (t nil))))


(defun safe-prove (wff)
  (assert (wff? wff))
  (prove wff))


