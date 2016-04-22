
(asdf:defsystem #:krr
  :serial t
  :depends-on (:optima :levenshtein :alexandria :fiveam :function-cache)
  :components ((:file "packages")
	       (:file "utils"         :depends-on ("packages"))
	       (:file "fol"           :depends-on ("utils"))
	       (:file "tableaux"      :depends-on ("fol"))
	       (:file "tableaux-test" :depends-on ("tableaux"))
	       (:file "vestidos"      :depends-on ("tableaux"))
	       (:file "sudoku"        :depends-on ("tableaux"))))


