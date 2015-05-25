(defun variable-p (x)
  (and (symbolp x)
       (equal (char (symbol-name x) 0) #\?)))

;sugestao de representacao da formula "∀y.P(x)∧∃x[P(y)∨Q(x)]":
;(and (forall '?y 'P(x)) (exists '?x (or (P(y) Q(x)))))
