
(asdf:defsystem #:krr
  :serial t
  :depends-on (:optima :levenshtein :alexandria)
  :components ((:file "packages")
	       (:file "utils"    :depends-on ("packages"))
	       (:file "fol"      :depends-on ("utils"))
	       (:file "tableaux" :depends-on ("fol"))
	       (:file "vestidos" :depends-on ("tableaux"))
	       (:file "sudoku"   :depends-on ("tableaux"))))


