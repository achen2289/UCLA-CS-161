(defun test-BFS()
    (and
        (equal NIL (BFS '()))
        (equal '(3) (BFS '(3)))
        (equal '(2 3) (BFS '(2 3)))
        (equal '(3 2) (BFS '(3 2)))
        (equal '(3 2 5) (BFS '(3 2 5)))
        (equal '(3 4 2 5 6 8) (BFS '((3 4) (2 5) (6 8))))
        (equal '(4 2 3 5 6 8 10 11) (BFS '(((2 3) 4) ((5)) ((6 (10 11) 8)))))
    )
)

(defun break-BFS()
    (and
        (equal '(3 4) (BFS '(4 (3))))
    )
)

(defun test-DFS()
    (and
        (equal NIL (DFS '()))
        (equal '(3) (DFS '(3)))
        (equal '(2 3) (DFS '(2 3)))
        (equal '(3 2) (DFS '(3 2)))
        (equal '(3 2 5) (DFS '(3 2 5)))
        (equal '(3 4 2 5 6 8) (DFS '((3 4) (2 5) (6 8))))
        (equal '(2 3 4 5 6 10 11 8) (DFS '(((2 3) 4) ((5)) ((6 (10 11) 8)))))
    )
)

(defun break-DFS()
    (and
        (equal '(3 4) (DFS '(4 3)))
    )
)

(defun test-DFID()
    (and
        (equal NIL (DFID '() 0))
        (equal '(3) (DFID '(3) 1))
        (equal '(Z Y X W) (DFID '((w x) (y z)) 2))
        (equal '(Z Y X W Z Y X W) (DFID '((w x) (y z)) 3))
        (equal '(C D C A D C B A) (DFID '((A (B)) C (D)) 3))
    )
)

(defun test-RIVER()
    (and
        (test-final-state)
        (test-next-state)
    )
)

(defun test-final-state()
    (and
        (final-state '(3 3 NIL))
        (not (final-state '(3 3 t)))
        (not (final-state NIL))
        (not (final-state '(3 3 NIL NIL)))
        (not (final-state '(1 2)))
        (not (final-state '(2)))
        (not (final-state '(3 2 NIL)))
    )
)

(defun test-next-state()
    (and
        (equal '((1 1 NIL)) (next-state '(3 3 T) 1 1))
        (equal '((0 1 NIL)) (next-state '(3 3 T) 0 1))
        (equal NIL (next-state '(3 3 T) 1 0))
        (equal '((0 2 NIL)) (next-state '(3 3 T) 0 2))
        (equal NIL (next-state '(3 3 T) 2 0))
        (equal '((3 1 T)) (next-state '(0 3 NIL) 0 1))
        (equal NIL (next-state '(0 3 NIL) 1 1))
        (equal NIL (next-state '(0 3 NIL) 2 0))
        (equal '((3 3 T)) (next-state '(0 1 NIL) 0 1))
    )
)

(defun test-all()
    (and 
        (test-DFS)
        (not (break-DFS))
        (test-BFS)
        (not (break-BFS))
        (test-DFID)
        (test-RIVER)
    )
)

(if (test-all) (print "Passed all") (print "Failed"))