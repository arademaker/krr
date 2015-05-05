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

(defpackage #:gps-1
  (:use #:cl #:utils))

(defpackage #:gps-2
  (:use #:cl #:utils))

(defpackage #:eliza
  (:use #:cl #:utils))

(defpackage #:katabank
  (:use #:cl #:utils))

(defpackage #:tableaux
  (:use #:cl #:utils :optima))
