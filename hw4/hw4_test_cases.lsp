(defun test-solver()
    (and
        (equal '(25 -24 23 22 21 -20 19 18 -17 16 15 -14 13 -12 11 -10 -9 8 -7 -6 -5 4 -3 2 1) (solve-cnf "./cnfs/f1/sat_f1.cnf"))
        (equal '(25 24 -23 22 21 20 -19 -18 17 -16 -15 14 -13 -12 11 10 9 -8 -7 -6 -5 -4 3 2 1) (solve-cnf "./cnfs/f2/sat_f2.cnf"))
        (equal '(22 21 -20 19 -18 17 16 -15 -14 -13 -12 -11 -10 -9 -8 7 -6 5 4 -3 -2 -1) (solve-cnf "./cnfs/f3/sat_f3.cnf"))
        (equal '(3 -2 -1) (sat? 3 '((1 -2 3) (-1) (-2 -3))))
        (equal nil (sat? 1 '((1) (-1))))
    )
)

(defun test-all()
    (and 
        (test-solver)
    )
)

(if (test-all) (print "Passed all") (print "Failed"))