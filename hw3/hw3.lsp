; Alex Chen
; Spring 2022

; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.

; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
(defun reload()
	(load "hw3.lsp")
)

;
; For loading a-star.lsp.
;
(defun load-a-star()
	(load "a-star.lsp")
)

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
	(reload)
	(load-a-star)
)

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
(defun sokoban(s h)
	(a* s #'goal-test #'next-states h)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
	(= v blank)
)

(defun isWall (v)
	(= v wall)
)

(defun isBox (v)
	(= v box)
)

(defun isKeeper (v)
	(= v keeper)
)

(defun isStar (v)
	(= v star)
)

(defun isBoxStar (v)
	(= v boxstar)
)

(defun isKeeperStar (v)
	(= v keeperstar)
)

; Helper function of getKeeperPosition
(defun getKeeperColumn (r col)
	(cond ((null r) nil)
		  (t (if (or (isKeeper (car r)) (isKeeperStar (car r))) col
				 (getKeeperColumn (cdr r) (+ col 1))
			 )
		  )
	)
)

; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).

; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (left) column is the zeroth column.
(defun getKeeperPosition (s row)
	(cond ((null s) nil)
		  (t (let (
			  	(x (getKeeperColumn (car s) 0)))
				(if x ;keeper is in this row
				(list x row)
				;otherwise move on
				(getKeeperPosition (cdr s) (+ row 1))
			)
			)
		  )
	)
)

; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
(defun cleanUpList (L)
	(cond ((null L) nil)
		  (t (let (
			  	(cur (car L))
		     	(res (cleanUpList (cdr L)))
		     )
	         (if cur (cons cur res)
		     res
		     )
		  	 )
		  )
	)
)

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
; (isBox and isKeeper false for all squares)
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
(defun goal-test (s)
	(cond ((null s) t)
		  ((goal-test-row (car s)) (goal-test (cdr s)))
		  (t nil)
	)
)

; Check if each number in each row represents a box or keeper.
; Return nil if so.
(defun goal-test-row (row)
	(cond ((null row) t)
		  ((isBox (car row)) nil)
		  ((isKeeper (car row)) nil)
		  (t (goal-test-row (cdr row)))
	)
)

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.

; Tries to move in each of the four directions, adds resulting state to returned list.
(defun next-states (s)
	(let* (
		(pos (getKeeperPosition s 0))
	 	(c (car pos))
	 	(r (cadr pos))
	 	(result (list
			(try-move s 0 r c) ; up
			(try-move s 1 r c) ; right
			(try-move s 2 r c) ; down
			(try-move s 3 r c) ; left
		)
	 	)
	)
    (cleanUpList result)
	)
)

; S is state
; r and c are 0-indexed row and column to retrieve
(defun get-square (S r c)
	(get-col (get-row S r) c)
)

; S is state
; r is row to retrieve
(defun get-row (S r)
	(cond ((null S) nil) ; row not in range, return nil
		  ((= r 0) (car S)) ; found row, return first
		  (t (get-row (cdr S) (- r 1))) ; recurse
	)
)

; r is row, c is column in row to retrieve
(defun get-col (r c)
	(cond ((null r) wall) ; column not in range, return wall
	      ((= c 0) (car r)) ; found column, return first
		  (t (get-col (cdr r) (- c 1))) ; recurse
	)
)

; Set row r and column c in S to v
; Once row is found, use set-col method
(defun set-square (S r c v)
	(cond ((null S) nil)
		  ((= r 0) (cons (set-col (car S) c v) (cdr S)))
		  (t (cons (car S) (set-square (cdr S) (- r 1) c v)))
	)
)

; Set column c in row r to v
(defun set-col (r c v)
	(cond ((null r) nil)
		  ((= c 0) (cons v (cdr r)))
		  (t (cons (car r) (set-col (cdr r) (- c 1) v)))
	)
)

; Try moving in each of the 4 directions
; If movement is valid (not moving into a wall or two consecutive blocks / walls), return new state
(defun try-move (S d r c)
	(let* (
		(coord (get-new-coord r c d)) ; move once in d
		(new_r (car coord))
		(new_c (cadr coord))
		(further-coord (get-new-coord new_r new_c d)) ; move twice in d
		(further_r (car further-coord))
		(further_c (cadr further-coord))
		(res_sq (get-square S new_r new_c))
		(further_sq (get-square S further_r further_c))
		(further_sq_blocked (or (= further_sq wall) (= further_sq box) (= further_sq boxstar)))
		(further_sq_free (not further_sq_blocked))
		(curr_sq_no_keeper (cond ((isKeeperStar (get-square S r c)) star)
								 (t blank)
						   )
		)
	)
	(
		cond ((= wall res_sq) nil) ; wall
			 ((= blank res_sq) (set-square (set-square S r c curr_sq_no_keeper) new_r new_c keeper)) ; move onto blank
			 ((= star res_sq) (set-square (set-square S r c curr_sq_no_keeper) new_r new_c keeperstar)) ; move onto goal
			 (t ; box in front
			 	(cond ((not further_sq_free) nil) ; box, then wall/second box in front
				 	  ((and (= box res_sq) (= blank further_sq)) (set-square (set-square (set-square S r c curr_sq_no_keeper) new_r new_c keeper) further_r further_c box))
					  ((and (= box res_sq) (= star further_sq)) (set-square (set-square (set-square S r c curr_sq_no_keeper) new_r new_c keeper) further_r further_c boxstar))
					  ((and (= boxstar res_sq) (= blank further_sq)) (set-square (set-square (set-square S r c curr_sq_no_keeper) new_r new_c keeperstar) further_r further_c box))
					  ((and (= boxstar res_sq) (= star further_sq)) (set-square (set-square (set-square S r c curr_sq_no_keeper) new_r new_c keeperstar) further_r further_c boxstar))
				)
			 )
	)
	)
)

; Get new coord when moving in direction d
(defun get-new-coord (r c d)
	(cond ((= d 0) (list (- r 1) c))
		  ((= d 1) (list r (+ c 1)))
		  ((= d 2) (list (+ r 1) c))
		  ((= d 3) (list r (- c 1)))
	)
)

; Trivial heuristic
(defun h0 (s)
	0
)

; Returns the number of misplaced boxes in s (counts occurence of "2" in s)
; Should be admissible
; If n out of n boxes are not currently on goal states, the keeper needs to move
; n times at the very minimum, so no underestimate occurs in this case
; If 1 box is on a goal state and the one behind it is not, the keeper needs
; to move at least once
(defun h1 (s)
	(cond ((null s) 0)
		  (t (+ (h1-row-helper (car s)) (h1 (cdr s))))
	)
)

; Returns number of misplaced boxes in row r
(defun h1-row-helper (r)
	(cond ((null r) 0)
		  ((= box (car r)) (+ 1 (h1-row-helper (cdr r))))
		  (t (h1-row-helper (cdr r)))
	)
)

; EXERCISE: Modify this h2 function to compute an
; admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.

; If any boxes pushed onto corner of two walls, return infinity
; If any boxes pushed onto a wall and no goal on that wall, return infinity
; Otherwise, return manhattan distance
(defun h2 (s)
	(let (
		(boxes (find-vals s 0 (list box boxstar)))
		(goals (find-vals s 0 (list star boxstar keeperstar)))
	)(
		cond ((stuck-in-corner s boxes) 1000)
			 ((stuck-on-edge s boxes) 1000)
			 (t (h-manhattan-dist boxes goals))
	)
	)
)

;; Find position of all val in vals
;; Takes in state and current row
;; Returns list of lists (list of coords)
(defun find-vals (s r vals)
	(cond ((null s) nil)
		  (t (append (find-vals-in-row (car s) r 0 vals) (find-vals (cdr s) (+ r 1) vals)))
	)
)

;; Find values in row
(defun find-vals-in-row (row r c vals)
	(cond ((null row) nil)
		  ((>= (count (car row) vals) 1) (cons (list r c) (find-vals-in-row (cdr row) r (+ c 1) vals)))
		  (t (find-vals-in-row (cdr row) r (+ c 1) vals))
	)
)

;; Returns the sum of the manhattan distances between each box and its closest goal
(defun h-manhattan-dist (boxes goals)
	(cond ((null boxes) 0)
		  (t (+ (h-manhattan-dist-helper (car boxes) goals) (h-manhattan-dist (cdr boxes) goals)))
	)
)

;; Returns the manhattan distance between box and the closest goal
(defun h-manhattan-dist-helper (box goals)
	(cond ((null goals) 10000)
		  (t (let* (
			  	(curr_goal (car goals))
			  	(goal_r (car curr_goal))
				(goal_c (cadr curr_goal))
				(box_r (car box))
				(box_c (cadr box))
				(delta_x (abs (- box_r goal_r)))
				(delta_y (abs (- box_c goal_c)))
		  	 )(
				min (+ delta_x delta_y) (h-manhattan-dist-helper box (cdr goals))
			 ))
		  )
	)
)

;; Check if current box is on goal - if so, recurse
;; Else check if current box is on a corner
;; Otherwise, recurse
(defun stuck-in-corner (s boxes)
	(cond ((null boxes) nil)
		  (t (let* (
			  (curr_box (car boxes))
			  (r (car curr_box))
			  (c (cadr curr_box))
			  (above (get-new-coord r c 0))
			  (above_r (car above))
			  (above_c (cadr above))
			  (above_sq (get-square s above_r above_c))
			  (right (get-new-coord r c 1))
			  (right_r (car right))
			  (right_c (cadr right))
			  (right_sq (get-square s right_r right_c))
			  (below (get-new-coord r c 2))
			  (below_r (car below))
			  (below_c (cadr below))
			  (below_sq (get-square s below_r below_c))
			  (left (get-new-coord r c 3))
			  (left_r (car left))
			  (left_c (cadr left))
			  (left_sq (get-square s left_r left_c))
			  (box_on_goal (= boxstar (get-square s r c)))
		  )(
			  cond (box_on_goal (stuck-in-corner s (cdr boxes)))
			  	   ((and (= wall above_sq) (= wall right_sq)) t)
			  	   ((and (= wall right_sq) (= wall below_sq)) t)
				   ((and (= wall below_sq) (= wall left_sq)) t)
				   ((and (= wall left_sq) (= wall above_sq)) t)
				   (t (stuck-in-corner s (cdr boxes)))
		  )
		  ))
	)
)

;; Returns if any of the boxes in boxes is stuck on an edge with no possible goal in that row
(defun stuck-on-edge (s boxes)
	(cond ((null boxes) nil)
		  (t (let* (
			  (curr_box (car boxes))
			  (r (car curr_box))
			  (c (cadr curr_box))
			  (above (get-new-coord r c 0))
			  (above_r (car above))
			  (above_c (cadr above))
			  (above_sq (get-square s above_r above_c))
			  (right (get-new-coord r c 1))
			  (right_r (car right))
			  (right_c (cadr right))
			  (right_sq (get-square s right_r right_c))
			  (below (get-new-coord r c 2))
			  (below_r (car below))
			  (below_c (cadr below))
			  (below_sq (get-square s below_r below_c))
			  (left (get-new-coord r c 3))
			  (left_r (car left))
			  (left_c (cadr left))
			  (left_sq (get-square s left_r left_c))
			  (box_on_goal (= boxstar (get-square s r c)))
		  )(
			  cond (box_on_goal (stuck-on-edge s (cdr boxes)))
			  	   ((and (row-is-wall s above_r) (row-no-goal s r)) t)
			  	   ((and (col-is-wall s right_c) (col-no-goal s c)) t)
				   ((and (row-is-wall s below_r) (row-no-goal s r)) t)
				   ((and (col-is-wall s left_c) (col-no-goal s c)) t)
				   (t (stuck-on-edge s (cdr boxes)))
		  )))
	)
)

;; Return if row r of state s is a wall
(defun row-is-wall (s r)
	(cond ((< r 0) t)
		  (t (let (
				(row (car (nthcdr r s)))
			 )(
				 row-is-wall-helper s row
			 ))
		  )
	)
)

;; If reaching an element now a row, return nil
;; Otherwise, recurse
(defun row-is-wall-helper (s row)
	(cond ((null row) t)
		  ((not (= wall (car row))) nil)
		  (t (row-is-wall-helper s (cdr row)))
	)
)

;; Return if there is no goal present in row r of state s
(defun row-no-goal (s r)
	(let (
		(row (car (nthcdr r s)))
	)(
		row-no-goal-helper row
	))
)

;; If current square is a goal or keeper+goal, row does have goal so return nil
;; If row contains a boxstar, this doesn't count as goal because is already occupied by other box
;; Otherwise, recurse
(defun row-no-goal-helper (row)
	(cond ((null row) t)
		  ((= star (car row)) nil)
		  ((= keeperstar (car row)) nil)
		  (t (row-no-goal-helper (cdr row)))
	)
)

;; Return if column c is a wall
(defun col-is-wall (s c)
	(cond ((null s) t)
		  ((< c 0) t)
		  ((not (= wall (get-square s 0 c))) nil)
		  (t (col-is-wall (cdr s) c))
	)
)

;; If current square is a goal or keeper+goal, col does have goal so return nil
;; If col contains a boxstar, this doesn't count as goal because is already occupied by other box
;; Otherwise, recurse
(defun col-no-goal (s c)
	(cond ((null s) t)
		  ((< c 0) t)
		  ((= star (get-square s 0 c)) nil)
		  ((= keeperstar (get-square s 0 c)) nil)
		  (t (col-no-goal (cdr s) c))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)

(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)

(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun