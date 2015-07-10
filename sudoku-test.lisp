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
