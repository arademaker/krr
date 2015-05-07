
(asdf:defsystem #:krr
  :serial t
  :depends-on (:optima :levenshtein :alexandria)
  :components ((:file "packages")
	       (:file "utils"    :depends-on ("packages"))
	       (:file "tableaux" :depends-on ("utils"))
	       (:file "vestidos" :depends-on ("tableaux"))
	       (:file "sudoku"   :depends-on ("tableaux"))))


