;; Author: Alexandre Rademaker

(defpackage #:utils
  (:use #:cl)
  (:export
   #:complemento
   #:mappend
   #:member-equal
   #:starts-with
   #:dbg
   #:dbg-on
   #:dbg-off
   #:dbg-indent
   #:find-all
   #:flatten
   #:random-elt
   #:find-all-if
   #:compose))
   
(defpackage #:fol
  (:use #:cl #:utils)
  (:export 
   #:variable?
   #:literal?
   #:preproc
   #:forall
   #:exists
   #:implies
   #:equiv
   #:to-cnf
   #:length-form))

(defpackage #:tableaux
  (:use #:cl #:utils #:optima #:fol)
  (:export
   #:prove
   #:make-formula
   #:formula-sign
   #:formula-frm
   #:equal?))

(defpackage #:tableaux-test
  (:use :cl :tableaux :utils :fol :it.bese.fiveam))
