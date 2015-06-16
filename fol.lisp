
(in-package :fol)

(defun variable? (x)
  (and (symbolp x)
       (equal (char (symbol-name x) 0) #\?)))


(defun op? (x)
  (and (symbolp x)
       (member x '(not and or implies exists forall equiv)
	       :test #'equal)))       


(defun function? (l)
  (let ((f (car l)))
    (and (symbolp f)
         (not (variable? f))
         (not (op? f))
         (every #'term? (cdr l)))))
				 
				 
(defun term? (a-form)
  (or (and (symbolp a-form)
           (not (op? a-form)))
      (variable? a-form)
      (function? a-form)))
      
      
(defun literal? (form) 
  (or (atom form)
      (and (equal (car form) 'not)
	   (literal? (cadr form)))))


(defun length-form (form &optional (n 0))
  (if (null form)
      n
      (if (atom (car form))
          (if (member (car form) '(implies and or equiv))
              (length-form (cdr form) (+ n 1))
              (length-form (cdr form) n))
          (length-form (cdr form)
		       (+ n (length-form (car form)))))))


(defun preproc (formula &optional (i 1))
  (cond 
    ((and formula
	  (atom formula)
	  (symbolp formula))
     (values formula i))
    ((and (= (length formula) 3)
	  (equal (car formula) 'equiv))
     (multiple-value-bind (f k)
	 (pre-aux (cdr formula) i)
       (values `(and (implies ,f) (implies ,(reverse f))) k)))	
    ((and (listp formula)
          (equal (car formula) 'not)
	  (= (length formula) 2))
     (multiple-value-bind (f k)
	 (preproc (cadr formula) i)
       (values `(not ,f) k)))
    ((and (listp formula)
	  (equal (car formula) 'implies)
	  (= (length formula) 3))
     (multiple-value-bind (f k)
	 (pre-aux (cdr formula) i)
       (values (cons 'implies f) k)))
    ((and (listp formula)
	  (member (car formula) '(and or) :test #'equal)
	  (= (length formula) 2))
     (preproc (cadr formula) i))
    ((and (listp formula)
	  (member (car formula) '(and or) :test #'equal)
	  (> (length formula) 2))
     (multiple-value-bind (f k)
	 (pre-aux (cdr formula) i)
       (values (reduce (lambda (x y) (list (car formula) x y)) f) k)))
    ((and (listp formula)
           (member (car formula) '(exists forall) :test #'equal)
          (variable? (cadr formula))
          (= (length formula) 3))
     (multiple-value-bind (f k)
	 (preproc (caddr formula) (+ i 1))
       (values (sublis `((,(cadr formula) . ,(intern (format nil "?X~a" i))))
		       `(,(car formula) ,(cadr formula) ,f))
	       k)))
    ((and (listp formula)
	  (> (length formula) 1)
	  (symbolp (car formula))
	  (not (equal (char (symbol-name (car formula)) 0) #\?))
	  (every #'term? (cdr formula)))
     (values formula i))	       
    (t (error "Invalid Formula ~a" formula)))) 
    
    
(defun pre-aux (frms i)
  (do ((f frms (cdr f))
       (k i)
       (result nil))
      ((null f) (values (reverse result) k))
      (multiple-value-bind (form j)
	(preproc (car f) k)
	  (progn (push form result)
		 (setf k j)))))    


(defun remove-implies (form)
  (cond ((atom form) 
	 form)
	((equal (car form) 'implies) 
	 `(or (not ,(remove-implies (cadr form)))
	      ,(remove-implies (caddr form))))
	(t
	 (mapcar #'remove-implies form))))


(defun move-not (form)
  (cond ((atom form) 
	 form)
	((equal (car form) 'or)
	 `(or ,(move-not (cadr form)) ,(move-not (caddr form))))
	((equal (car form) 'and)
	 `(and ,(move-not (cadr form)) ,(move-not (caddr form))))
	((equal (car form) 'not)
	 (cond ((atom (cadr form))
		form)
	       ((equal (caadr form) 'and)
		`(or ,(move-not `(not ,(cadadr form))) ,(move-not `(not ,(car (cddadr form))))))
	       ((equal (caadr form) 'or)
		`(and ,(move-not `(not ,(cadadr form))) ,(move-not `(not ,(car (cddadr form))))))
	       ((equal (caadr form) 'not)
		(move-not (cadadr form)))))))


(defun dist-and-over-or (form)
  (cond ((literal? form) 
	 form)
	((equal (car form) 'and)
	 `(and ,(dist-and-over-or (cadr form)) ,(dist-and-over-or (caddr form))))
	((equal (car form) 'or)
	 (cond ((eval (cons 'and (mapcar #'literal? (cdr form))))
		form)
	       ((and (not (literal? (cadr form))) (equal (caadr form) 'and))
		`(and ,(dist-and-over-or `(or ,(cadadr form) ,(caddr form)))
		      ,(dist-and-over-or `(or ,(car (cddadr form)) ,(caddr form)))))
	       ((and (not (literal? (caddr form))) (equal (caaddr form) 'and))
		`(and ,(dist-and-over-or `(or ,(cadr form) ,(car (cdaddr form))))
		      ,(dist-and-over-or `(or ,(cadr form) ,(cadr (cdaddr form))))))
	       (t
		(let ((a (dist-and-over-or (cadr form)))
		      (b (dist-and-over-or (caddr form))))
		  (if (and (equal a (cadr form)) (equal b (caddr form)))
		      form
		      (dist-and-over-or `(or ,a ,b)))))))))


(defun to-cnf (form)
  (dist-and-over-or (move-not (remove-implies form))))


;; skolemization

(defun skolemization (form &optional (l nil))
  (cond ((and (equal (car form) 'exists)
              (null l))
         (sublis `((,(cadr form) . ,(gensym)))
         	 (skolemization (caddr form))))
	((equal (car form) 'exists)
	 (sublis `((,(cadr form) . ,(cons (gensym) l)))
		 (skolemization (caddr form) l)))
	((equal (car form) 'forall)
         `(forall ,(cadr form) ,(skolemization (caddr form) (push (cadr form) l))))
        ((member (car form) '(and or implies) :test #'equal)
         `(,(car form) ,(skolemization (cadr form) l) ,(skolemization (caddr form) l)))
	(t form)))


;; unification

(defun lookup (a env)
  (cdr (assoc a env)))


(defun chasevar (a env)
  (let ((b (car (lookup a env))))
    (if b
	(chasevar b env)	        
	a)))


(defun occs (a term env)
  (cond 
    ((function? term) 
     (occsl (cdr term)))
    ((variable? term) 
     (or (equal a term) 
	 (occsl (lookup term env))))
    (t
     nil)))


(defun occsl (a term_list env)
  (if (null term_list) 
      nil
      (or (occs a (car term_list) env) 
	  (occsl a (cdr term_list) env))))


(defun unify-var (a term env)
  (cond 
    ((variable? a)
     (if (equal a term)
	 env
	 (cons (list a term) env)))
    (t (unify-term a term env))))


(defun unify-term (ts us env)
  (cond
    ((variable? ts) 
     (unify-var (chasevar ts env) (chasevar us env) env))
    ((variable? us) 
     (unify-var (chasevar us env) (chasevar ts env) env))
    ((and (function? ts) (function? us))
     (if (equal (car ts) (car us)) 
	 (unify-terms (cdr ts) (cdr us) env)
	 env))))


(defun unify-terms (ts us env)
  (if (and (null ts) (null us))
      env
      (unify-terms (cdr ts) (cdr us) (unify-term (car ts) (car us) env))))


(defun unify (P1 P2 &optional (env nil))
  (if (equal (car P1) (car P2))
      (unify-terms (cdr P1) (cdr P2) env)
      env))
