;;
(in-package :utils)

;; (setf (symbol-function 'find-all-if) #'remove-if-not)

(defun compose (f g)
  #'(lambda (x) (funcall f (funcall g x))))

;; (defun complemento (fn)
;;   (lambda (&rest args) (not (apply fn args))))

;; (defun find-all (item sequence &rest keyword-args
;; 		 &key (test #'eql) test-not &allow-other-keys)
;;   (if test-not
;;       (apply #'remove item sequence
;; 	     :test-not (complement test-not) keyword-args)
;;       (apply #'remove item sequence
;; 	     :test (complement test) keyword-args)))

(defun mappend (fn the-list)
  (apply #'append (mapcar fn the-list)))

(defun member-equal (item a-list)
  (member item a-list :test #'equal))

(defun starts-with (a-list x)
  (and (consp a-list) 
       (eql (first a-list) x)))

(defun flatten (the-list)
  (mappend #'mklist the-list))

(defun mklist (x)
  (if (listp x)
      x
      (list x)))

(defun random-elt (choices)
  (elt choices (random (length choices))))


;; The Debugging Output Facility

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (format *debug-io* "~&~?" format-string args)))

(defun dbg-on (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun dbg-off (&rest ids)
  "Stop dbg on the ids.  With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (format *debug-io* "~&~V@T~?" (* 2 indent) format-string args)))

