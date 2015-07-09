
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

(test input-tab
      ;; caso em que há elementos maiores que 9
      (is (equal nil (input-tab
		      '(0 10 8 0 0 0 7 0 0
			0 0 0 3 0 0 2 0 0
			0 7 0 0 0 0 0 0 0
			0 0 0 0 7 1 0 0 0
			6 0 0 0 0 0 0 4 0
			3 0 0 0 0 0 0 0 0
			4 0 0 5 0 0 0 0 3
			0 2 0 0 8 0 0 0 0
			0 0 0 0 0 0 0 6 0))))
      ;; caso em que há mais que 81 elementos 
      (is (equal nil (input-tab
		      '(0 1 8 0 0 0 7 0 0
			0 0 0 3 0 0 2 0 0
			0 7 0 0 0 0 0 0 0
			0 0 0 7 1 0 0 0 6
			0 0 0 0 0 0 4 0 3
			0 0 0 0 0 0 0 0 4
			0 0 5 0 0 0 0 3 0
			2 0 0 8 0 0 0 0 0
			0 0 0 0 0 0 6 0 0 1))))
      ;; caso normal, retorna triplas do tipo (linha coluna valor)
      (is (equal '((1 2 1) (1 3 8) (1 7 7) (2 4 3) (2 7 2) 
		   (3 2 7) (4 5 7) (4 6 1) (5 1 6) (5 8 4)
		   (6 1 3) (7 1 4) (7 4 5) (7 9 3) (8 2 2) (8 5 8) (9 8 6))
                 (input-tab
                  '(0 1 8 0 0 0 7 0 0
		    0 0 0 3 0 0 2 0 0
		    0 7 0 0 0 0 0 0 0
		    0 0 0 0 7 1 0 0 0
		    6 0 0 0 0 0 0 4 0
		    3 0 0 0 0 0 0 0 0
		    4 0 0 5 0 0 0 0 3
		    0 2 0 0 8 0 0 0 0
		    0 0 0 0 0 0 0 6 0)))))

