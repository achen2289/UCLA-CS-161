;;; Alex Chen, 005299047
;;; Spring 2022

;;; If no children, return atom
;;; Otherwise, if car is atom, make car head
;;; Otherwise append car to the result of BFS on cdr
(defun BFS(TREE)
      ;; TREE is a list representation of a tree.
      ;; Returns list of terminal nodes of BFS on tree, from left-to-right
      (cond ((null TREE) NIL)
            ((atom TREE) (list TREE))
            (t (let 
                  ((left_child (car TREE)))
                  (cond ((atom left_child) (cons left_child (BFS (cdr TREE))))
                        (t (BFS (append (cdr TREE) left_child)))
                  )
                  )
            )
      )
)

;;; If no children exist, return atom
;;; Otherwise, return result of DFS on each child of root, appended together
(defun DFS(TREE)
    ;; TREE is a list representation of a tree
    ;; Returns list of terminal nodes of DFS on tree, from left-to-right
    (cond ((null TREE) NIL)
          ((atom TREE) (list TREE))
          (t (append (DFS (car TREE)) (DFS (cdr TREE))))
    )
)

(defun DFID(TREE MD)
      ;; TREE is a list representation of a tree, MD is max depth of search
      ;; Return list of terminal nodes reached by iterative deepening from right-to-left
      (DFS_HELPER_1 TREE 0 MD)
)

;; Appends nodes reached by DFS on tree at CD using one helper function 
;; and nodes reached by DFS on tree from CD+1 to MD using this function (recurse)
;; End when current depth exceeds max depth
(defun DFS_HELPER_1(TREE CD MD)
    ;; TREE is a list representation of a tree, CD is the current depth of search, MD is max depth of search
    ;; Returns list of terminal nodes reached from DFS on TREE ending at depth CD, CD+1, ..., MD
    (cond ((> CD MD) NIL)
          (t (append (DFS_HELPER_2 TREE 0 CD) (DFS_HELPER_1 TREE (+ CD 1) MD)))
    )
)

;; Do search on cdr of TREE and append with result of search on car of TREE
;; Keep track of current level of search so can terminate search when reaching MAX_LVL depth
(defun DFS_HELPER_2(TREE CURR_LVL MAX_LVL)
    ;; TREE is the list representation of a tree, CURR_LVL is current level of search, MAX_LVL is level to stop at
    ;; Returns DFS on TREE with max depth of MAX_LVL
    (cond ((> CURR_LVL MAX_LVL) NIL)
          ((null TREE) NIL)
          ((atom TREE) (list TREE))
          (t (let
                ((new_lvl (+ CURR_LVL 1)))
                (append (DFS_HELPER_2 (cdr TREE) CURR_LVL MAX_LVL) (DFS_HELPER_2 (car TREE) new_lvl MAX_LVL))
             )
          )    
    )
)

; These functions implement a depth-first solver for the River-Boat
; problem. In this problem, three members from Group-X, denoted XXX,
; and three members from Group-O, denoted OOO, are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more O's on one side of the river than X's.

; In this implementation, a state is represented by a single list
; (#X #O side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. #X and #O represent the number of X's and
; O's on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three X's, three O's, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state(s)
  (equal s '(3 3 NIL))
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; X's to move (m), and a number of O's to move (c). It returns a
; list containing the state that results from moving that number of X's
; and O's from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more O's than X's on either side of the river, or because
; it would move more X's or O's than are on this side of the
; river) it returns NIL. There can be 0 X's and 1+ 0's on a side, however.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).

; Assumes s is well formed input
; Assumes m and c meet requirements (sum is at most 2)
(defun next-state (s m c)
  (let*
    (
      (num_x (car s)) 
      (num_o (cadr s)) 
      (east (caddr s))
      (num_x_left (- num_x m))
      (num_o_left (- num_o c))
      (num_x_other (- 3 num_x_left))
      (num_o_other (- 3 num_o_left))
    )
    (cond ((or (< num_x_left 0) (< num_o_left 0)) NIL)
          ((and (> num_x_left 0) (< num_x_left num_o_left)) NIL)
          ((and (> num_x_other 0) (< num_x_other num_o_other)) NIL)
          (t (list (list num_x_other num_o_other (not east))))
    )
  )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.

; Note: assumes s is well-formed
(defun succ-fn (s)
    (append
        (cond ((next-state s 0 2) (next-state s 0 2)) ; take 0 X's, 2 0's
              (t NIL)
        )
        (cond ((next-state s 1 1) (next-state s 1 1)) ; take 1 of each
              (t NIL)
        )
        (cond ((next-state s 2 0) (next-state s 2 0)) ; take 2 X's, 0 0's
              (t NIL)
        )
        (cond ((next-state s 1 0) (next-state s 1 0)) ; take 1 X, 0 0's
              (t NIL)
        )
        (cond ((next-state s 0 1) (next-state s 0 1)) ; take 0 X's, 1 0
              (t NIL)
        )
    )
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  (cond ((null states) NIL)
        ((equal s (car states)) t)
        (t (on-path s (cdr states)))
  )
)

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
  ;; If states is null, then return NIL because no more to process
  ;; Else, process first element of states (car states)
  ;; If the car has been visited already, return NIL
  ;; Elif car is the final-state, return the path containing car as first element in list
  ;; Else, DFS on car and also DFS on cdr
  (cond ((null states) NIL)
        ;((on-path (car states) path) NIL)
        ;((final-state (car states)) (append (list (car states)) path))
        (t (let*
            ((car_res (mc-dfs (car states) path)))
            (cond (car_res car_res)
                  (t (mult-dfs (cdr states) path))
            )
           )
        )
  )
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.

; Note: checking if S is goal state or if DFS does not revisit node is delegated 
; to mult-dfs
(defun mc-dfs (s path)
  (cond ((final-state s) (cons s path))
        ((on-path s path) NIL)
        (t (mult-dfs (succ-fn s) (cons s path)))
  )
)
  ;)
  ;(mult-dfs (list s) path)


; Function execution examples

; Applying this operator would result in an invalid state, with more O's
; than X's on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one O and zero X on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))
