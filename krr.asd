
(asdf:defsystem #:krr
  :serial t
  :depends-on (:optima :levenshtein :alexandria)
  :components ((:file "packages")
	       (:file "utils"    :depends-on ("packages"))
	       (:file "tableaux" :depends-on ("utils"))
	       (:file "tableaux-vestidos" :depends-on ("tableaux"))
	       (:file "tableaux-opt"      :depends-on ("tableaux"))
	       (:file "tableaux-safe"     :depends-on ("tableaux"))
	       (:file "tableaux-ext"      :depends-on ("tableaux" "tableaux-vestidos"))
	       (:file "sudoku"            :depends-on ("tableaux-ext"))))


