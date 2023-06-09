(defun test-get-square()
    (and
        (= wall (get-square '((2 2 2) (2 2 2) (2 2 2)) 3 2))
        (= wall (get-square '((2 2 2) (2 2 2) (2 2 2)) 0 3))
        (= wall (get-square '((2 2 2) (2 2 2) (2 2 2)) -1 0))
        (= box (get-square '((2 2 2) (2 2 2) (2 2 2)) 1 1))
        (= wall (get-square nil 1 1))
        (= keeper (get-square '((2 2 2) (2 2 3) (2 2 2)) 1 2))
    )
)

(defun test-next-states()
    ;; m0 tests no possible movements
    ;; m1 tests all possible movements (no box)
    ;; s1 tests keeper moves a box
    ;; s2 tests keeper moves onto a goal
    ;; s3 tests keeper moves off of a goal
    ;; s4 tests keeper moves a box off a goal
    ;; s5 tests keeper moves a box off a goal onto another goal
    ;; s6 tests keeper moves a box onto a goal
    ;; s7 tests keeper attempts to move a box
    (let (
        (m0 '((1 1 1) (1 3 1) (1 1 1)))
        (m0_res nil
        )
        (m1 '((0 0 0) (0 3 0) (0 0 0)))
        (m1_res '(((0 3 0) (0 0 0) (0 0 0))
                  ((0 0 0) (0 0 3) (0 0 0))
                  ((0 0 0) (0 0 0) (0 3 0))
                  ((0 0 0) (3 0 0) (0 0 0)))
        )
        (s1 '((1 1 1 1 1) (1 4 0 0 1) (1 0 2 0 1)  (1 0 3 0 1)  (1 0 0 0 1)  (1 1 1 1 1)))
        (s1_res '(((1 1 1 1 1) (1 4 2 0 1) (1 0 3 0 1) (1 0 0 0 1) (1 0 0 0 1) (1 1 1 1 1))
                  ((1 1 1 1 1) (1 4 0 0 1) (1 0 2 0 1) (1 0 0 3 1) (1 0 0 0 1) (1 1 1 1 1)) 
                  ((1 1 1 1 1) (1 4 0 0 1) (1 0 2 0 1) (1 0 0 0 1) (1 0 3 0 1) (1 1 1 1 1)) 
                  ((1 1 1 1 1) (1 4 0 0 1) (1 0 2 0 1) (1 3 0 0 1) (1 0 0 0 1) (1 1 1 1 1)))
        )
        (s2 '((1 1 1 1 1) (1 0 0 4 1) (1 0 2 3 1) (1 0 0 0 1) (1 0 0 4 1) (1 1 1 1 1)))
        (s2_res '(((1 1 1 1 1) (1 0 0 6 1) (1 0 2 0 1) (1 0 0 0 1) (1 0 0 4 1) (1 1 1 1 1))
                  ((1 1 1 1 1) (1 0 0 4 1) (1 0 2 0 1) (1 0 0 3 1) (1 0 0 4 1) (1 1 1 1 1))
                  ((1 1 1 1 1) (1 0 0 4 1) (1 2 3 0 1) (1 0 0 0 1) (1 0 0 4 1) (1 1 1 1 1)))
        )
        (s3 '((1 1 1 1 1) (1 0 0 6 1) (1 0 2 0 1) (1 0 0 0 1) (1 4 0 4 1) (1 1 1 1 1)))
        (s3_res '(((1 1 1 1 1) (1 0 0 4 1) (1 0 2 3 1) (1 0 0 0 1) (1 4 0 4 1) (1 1 1 1 1))
                  ((1 1 1 1 1) (1 0 3 4 1) (1 0 2 0 1) (1 0 0 0 1) (1 4 0 4 1) (1 1 1 1 1)))
        )
        (s4 '((1 1 1 1 1) (1 0 2 4 1) (1 0 0 0 1) (1 0 0 0 1) (1 0 5 3 1) (1 1 1 1 1)))
        (s4_res '(((1 1 1 1 1) (1 0 2 4 1) (1 0 0 0 1) (1 0 0 3 1) (1 0 5 0 1) (1 1 1 1 1))
                  ((1 1 1 1 1) (1 0 2 4 1) (1 0 0 0 1) (1 0 0 0 1) (1 2 6 0 1) (1 1 1 1 1)))
        )
        (s5 '((1 1 1 1 1) (1 0 2 4 1) (1 0 0 0 1) (1 0 0 0 1) (1 4 5 3 1) (1 1 1 1 1)))
        (s5_res '(((1 1 1 1 1) (1 0 2 4 1) (1 0 0 0 1) (1 0 0 3 1) (1 4 5 0 1) (1 1 1 1 1))
                  ((1 1 1 1 1) (1 0 2 4 1) (1 0 0 0 1) (1 0 0 0 1) (1 5 6 0 1) (1 1 1 1 1)))
        )
        (s6 '((1 1 1 1 1) (1 0 2 4 1) (1 0 0 0 1) (1 0 0 0 1) (1 4 2 3 1) (1 1 1 1 1)))
        (s6_res '(((1 1 1 1 1) (1 0 2 4 1) (1 0 0 0 1) (1 0 0 3 1) (1 4 2 0 1) (1 1 1 1 1))
                  ((1 1 1 1 1) (1 0 2 4 1) (1 0 0 0 1) (1 0 0 0 1) (1 5 3 0 1) (1 1 1 1 1)))
        )
        (s7 '((1 1 1 1 1) (1 0 2 4 1) (1 0 0 0 1) (1 0 0 0 1) (1 5 2 3 1) (1 1 1 1 1)))
        (s7_res '(((1 1 1 1 1) (1 0 2 4 1) (1 0 0 0 1) (1 0 0 3 1) (1 5 2 0 1) (1 1 1 1 1)))
        )
    )
    (and
        (equal (next-states m0) m0_res)
        (equal (next-states m1) m1_res)
        (equal (next-states s1) s1_res)
        (equal (next-states s2) s2_res)
        (equal (next-states s3) s3_res)
        (equal (next-states s4) s4_res)
        (equal (next-states s5) s5_res)
        (equal (next-states s6) s6_res)
        (equal (next-states s7) s7_res)
    )
    )
)

(defun test-find-vals()
    (let (
        (m0 '((1 1 1) (1 3 1) (1 1 1)))
        (m0_vals '(box))
        (m0_res '())
        (m1 '((1 2 1) (1 3 1) (1 1 1)))
        (m1_vals '(2))
        (m1_res '((0 1)))
        (m2 '((1 2 1) (1 3 1) (1 1 1)))
        (m2_vals (list 2 5))
        (m2_res '((0 1)))
        (m3 '((1 2 1) (1 5 1) (1 1 1)))
        (m3_vals (list 2 5))
        (m3_res '((0 1) (1 1)))
        (m4 '((1 2 1) (1 5 1) (1 1 5)))
        (m4_vals (list 2 5))
        (m4_res '((0 1) (1 1) (2 2)))
        (m5 '((4 5 6) (0 5 2) (1 1 5)))
        (m5_vals (list 4 5 6))
        (m5_res '((0 0) (0 1) (0 2) (1 1) (2 2)))
    )
    (and
        (equal (find-vals m0 0 m0_vals) m0_res)
        (equal (find-vals m1 0 m1_vals) m1_res)
        (equal (find-vals m2 0 m2_vals) m2_res)
        (equal (find-vals m3 0 m3_vals) m3_res)
        (equal (find-vals m4 0 m4_vals) m4_res)
        (equal (find-vals m5 0 m5_vals) m5_res)
    )
    )
)

(defun test-h0()
    (and
        (= (h0 '()) 0)
        (= (h0 '((1 2 3))) 0)
    )
)

(defun test-h1()
    (let (
        (t0 '())
        (m0 '((1 1 1) (1 3 1) (1 1 1)))
        (m1 '((0 0 0) (0 3 0) (0 0 0)))
        (s1 '((1 1 1 1 1) (1 4 0 0 1) (1 0 2 0 1)  (1 0 3 0 1)  (1 0 0 0 1)  (1 1 1 1 1)))
        (s2 '((1 1 2 1 1) (1 0 0 4 1) (1 0 2 3 1) (1 0 0 0 1) (1 0 0 4 1) (1 1 1 1 2)))
        (s3 '((1 1 1 1 1) (1 0 0 6 1) (1 0 2 0 1) (1 0 0 0 1) (1 4 0 4 1) (1 1 1 1 1)))
        (s4 '((1 1 1 1 1) (1 0 2 4 1) (1 0 0 0 1) (1 0 0 0 1) (1 0 5 3 1) (1 1 1 1 1)))
        (s5 '((1 1 1 1 1) (1 0 2 4 1) (1 0 0 0 1) (1 0 0 0 1) (1 4 5 3 1) (1 1 1 1 1)))
        (s6 '((1 1 1 1 1) (1 0 2 4 1) (1 0 0 0 1) (1 0 0 0 1) (1 4 2 3 1) (1 1 1 1 1)))
        (s7 '((1 1 1 1 1) (1 0 2 4 1) (1 0 0 0 1) (1 0 0 0 1) (1 5 2 3 1) (1 1 1 1 1)))
    )
    (and
        (= (h1 t0) 0)
        (= (h1 m0) 0)
        (= (h1 m1) 0)
        (= (h1 s1) 1)
        (= (h1 s2) 3)
        (= (h1 s3) 1)
        (= (h1 s4) 1)
        (= (h1 s5) 1)
        (= (h1 s6) 2)
        (= (h1 s7) 2)
    )
    )
)

(defun test-h2()
    (let (
        (s1 '((1 1 1 1 1) (1 4 1 0 1) (1 1 2 0 1)  (1 0 3 0 1)  (1 0 4 0 1)  (1 1 0 1 1)))
        (s1_res 4999)
        (s2 '((0 0 0 4 0) (0 0 0 2 4) (0 2 0 0 0) (0 0 0 2 0) (0 0 0 0 0) (0 0 4 0 0)))
        (s2_res 8)
        (s3 '((1 5 1 1 1) (1 0 0 6 1) (1 0 5 0 1) (1 0 0 0 1) (1 4 0 4 1) (1 1 5 1 1)))
        (s3_res 0)
        (s4 '((1 5 1 1 1) (1 0 0 6 1) (1 0 5 0 1) (1 0 2 0 1) (1 4 0 4 1) (1 1 5 1 1)))
        (s4_res 1)
        (s5 '((0 0 0 0 0) (1 5 1 2 1) (1 0 0 6 1) (1 0 5 0 1) (1 0 0 0 1) (1 4 0 4 1) (1 1 5 1 1)))
        (s5_res 1)
        (s6 '((1 1 5 1 1) (0 2 0 1 4) (0 0 0 2 2) (4 6 0 0 0)))
        (s6_res 5)
    )
    (and
        (= (h2 s1) s1_res)
        (= (h2 s2) s2_res)
        (= (h2 s3) s3_res)
        (= (h2 s4) s4_res)
        (= (h2 s5) s5_res)
        (= (h2 s6) s6_res)
    )
    )
)

(defun test-all()
    (and 
        (test-get-square)
        (test-next-states)
        (test-find-vals)
        (test-h0)
        (test-h1)
        (test-h2)
    )
)

(if (test-all) (print "Passed all") (print "Failed"))