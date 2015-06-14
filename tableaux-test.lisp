
(in-package :tableaux-test)

(test tableaux-1
  "test the tableaux prover"
  (is (prove '(and A B)) (list (make-formula 'false 'A)
			       (make-formula 'false 'B))))

(test length-1
  "test the length of a formula"
  (is (= 2 (length-form 'A)))
  (is (= 3 (length-form '(implies (and A B) (and C D))))) 
  (is (= 7 (length-form '(and (and (implies A B) (implies B A)) (and (implies B C) (implies C B))))))))

