
(in-package :krr-user)


(def-suite tableaux-suite :description "The tableaux test suite.")

(in-suite tableaux-suite)

(test tableaux-1
  "test the tableaux prover"
  (is (not (null (prove '(and A B)))))
  (is (null (prove '(implies (not (not A)) A)))))

(test length-1
  "test the length of a formula"
  (is (= 1 (length-form 'A)))
  (is (= 3 (length-form '(implies (and A B) (and C D))))) 
  (is (= 7 (length-form '(and (and (implies A B) (implies B A))
			      (and (implies B C) (implies C B))))))
  (is (= 1 (length-form '(implies A B)))))


