
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
      
      
(defun predicate? (x)
  (and (listp x)
       (symbolp (car x))
       (not (variable? (car x)))
       (not (op? (car x)))
       (> (length x) 1)
       (every #'term? (cdr x))))      
      
      
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
		       
		       
(defun wff? (formula)
  (labels ((big-and (formula)
    (reduce #'(lambda (x y) (and x y))
           (mapcar #'wff? formula))))
 (if         
  (or
    (and formula
         (atom formula)
	 (symbolp formula))
         (predicate? formula)
    (and (listp formula)
	 (= (length formula) 3)
         (equal (car formula) 'equiv)
         (big-and formula))
    (and (listp formula)
         (equal (car formula) 'not)
         (= (length formula) 2)
	 (big-and formula))
    (and (listp formula)
	 (equal (car formula) 'implies)
	 (= (length formula) 3)
	 (big-and formula))
    (and (listp formula)
         (member (car formula) '(and or) :test #'equal)
	 (> (length formula) 1)
	 (big-and formula))
    (and (listp formula)
         (member (car formula) '(exists forall) :test #'equal)
	 (variable? (cadr formula))
         (= (length formula) 3)
         (wff? (caddr formula))))
  t
  (error "Invalid Formula ~a" formula))))


(defun preproc-fol (formula)
  (labels ((map-preproc (formula)
	      (mapcar #'preproc-fol (cdr formula)))) 
  (cond 
    ((or (atom formula) (predicate? formula))
     formula)
    ((equal (car formula) 'equiv)
     (let ((forms (map-preproc formula)))
       `(and ,(cons 'implies forms) ,(cons 'implies (reverse forms)))))
    ((equal (car formula) 'not)
     `(not ,(preproc-fol (cadr formula))))
    ((equal (car formula) 'implies)
     (cons 'implies (map-preproc formula)))
    ((and (member (car formula) '(or and) :test #'equal)
          (> (length formula) 2))		
     (reduce #'(lambda (x y) `(,(car formula) ,(preproc-fol x) ,(preproc-fol y)))
	     (cdr formula)))
    ((member (car formula) '(or and) :test #'equal)
     (preproc-fol (cadr formula)))		
    ((member (car formula) '(exists forall) :test #'equal)
     `(,(car formula) ,(cadr formula) ,(preproc-fol (caddr formula))))
    (t nil))))


(defun remove-implies (form)
  (cond (or (atom form) (predicate? form))
	 form)
	((equal (car form) 'implies) 
	 `(or (not ,(remove-implies (cadr form)))
	      ,(remove-implies (caddr form))))
	(t
	 (mapcar #'remove-implies form))))


(defun move-not (form)
  (cond ((or (atom form) (predicate? form))
	 form)
	((member (car form) '(and or) :test #'equal)
  	(cons (car form) (mapcar #'move-not (cdr form))))
	((member (car form) '(exists forall) :test #'equal)
  	`(,(car form) ,(cadr form) ,(move-not (caddr form))))
	((equal (car form) 'not)
	 (cond ((atom (cadr form))
		form)
	       ((equal (caadr form) 'and)
		(cons 'or (mapcar #'(lambda (x) (move-not `(not ,x))) (cdadr form))))
	       ((equal (caadr form) 'or)
		(cons 'and (mapcar #'(lambda (x) (move-not `(not ,x))) (cdadr form))))
	       ((equal (caadr form) 'exists)
		`(forall ,(cadadr form) ,(move-not `(not ,(caddr (cadr form))))))
         ((equal (caadr form) 'forall)
		`(exists ,(cadadr form) ,(move-not `(not ,(caddr (cadr form))))))		
	       ((equal (caadr form) 'not)
		(move-not (cadadr form)))))))


(defun dist-and-over-or (form)
  (cond ((or (literal? form) (predicate? form))
	 form)
	((equal (car form) 'not)
	 `(not ,(dist-and-over-or (cadr form))))
	((member (car form) '(exists forall) :test #'equal)
	 `(,(car form) ,(cadr form) ,(dist-and-over-or (caddr form))))
	((equal (car form) 'and)
	 (cons 'and (mapcar #'dist-and-over-or (cdr form))))
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
		      
		      
(defun move-universals (form)
  (labels 
    ((remove-forall (form variables)
      (cond
        ((or (atom form) (predicate? form))
         (values form variables))
	((and (listp form)
              (equal (car form) 'forall))
	 (multiple-value-bind (f v)
	   (remove-forall (caddr form) (push (cadr form) variables))
	   (values f v)))
	(t (remove-aux form variables))))
    (remove-aux (form variables)
      (do ((f form (cdr f))
           (vars variables)
	   (results nil))
	  ((null f) (values (reverse results) vars))
	  (multiple-value-bind (formula v)
	    (remove-forall (car f) vars)
	     (push formula results)
	     (setf vars v)))))
    (multiple-value-bind (new-form vars)
      (remove-forall form nil)
        (do ((v vars (cdr v))
             (f new-form `(forall ,(car v) ,f)))
            ((null v) f)))))


(defun to-cnf (form)
  (dist-and-over-or (move-universals (skolemization (standardize (move-not (remove-implies form)))))))


;; skolemization

(defun skolemization (form &optional (l nil))
  (cond ((and (listp form)
	      (equal (car form) 'exists)
              (null l))
         (sublis `((,(cadr form) . ,(intern (gensym))))
         	 (skolemization (caddr form))))
	((and (listp form) 
	      (equal (car form) 'exists))
	 (sublis `((,(cadr form) . ,(cons (intern (gensym)) l)))
		 (skolemization (caddr form) l)))
	((and (listp form)
	      (equal (car form) 'forall))
	 (append `(forall ,(cadr form))
	          (mapcar #'(lambda (x) (skolemization x (push (cadr form) l)))
        		  (cddr form))))
        ((and (listp form) 
	      (member (car form) '(and or implies) :test #'equal))
	 (cons (car form) (mapcar #'(lambda (x) (skolemization x l)) (cdr form))))
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
