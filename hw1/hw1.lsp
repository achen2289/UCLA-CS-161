;;;; Alex Chen, 005299047
;;;; CS 161, Spring 2022

;;; If TREE is NIL, then N was not found.
;;; Elif TREE is an atom, then last element has been found, so check if is equal to N.
;;; Otherwise, check if N is in the two children or root of TREE.

(defun TREE-CONTAINS(N TREE)
    ;; N is a number, TREE is an ordered tree
    ;; Returns T or NIL representing whether or not N is in TREE
    (cond ((null TREE) NIL) ; base case, null tree
          ((atom TREE) (equal N TREE))
          (t (or (TREE-CONTAINS N (car TREE)) (TREE-CONTAINS N (cadr TREE)) (TREE-CONTAINS N (cddr TREE))))
    )
)

;;; If TREE is NIL, then there is no possible maximum.
;;; Elif TREE is an atom, then this is the max element and the last element arrived upon.
;;; Else look for max in the right child, root, the left child.

(defun TREE-MAX(TREE)
    ;; TREE is an ordered tree
    ;; Returns the maximum number in TREE, and NIL if TREE is originally empty
    (cond ((null TREE) NIL) ; only reached if empty list passed in
          ((atom TREE) TREE)
          ((not (null (cddr TREE))) (TREE-MAX (cddr TREE)))
          ((not (null (cadr TREE))) (TREE-MAX (cadr TREE)))
          (t (TREE-MAX (car TREE)))
    )
)

;;; If TREE is NIL, then return empty list.
;;; Elif TREE is an atom, then return a list of that number.
;;; Else return the post-ordering of the car, cddr, and cadr of tree appended together (children + root).

(defun TREE-ORDER(TREE)
    ;; TREE is an ordered tree
    ;; Returns the post-ordering of tree
    (cond ((null TREE) NIL)
          ((atom TREE) (list TREE))
          (t (append (TREE-ORDER (car TREE)) (TREE-ORDER (cddr TREE)) (TREE-ORDER (cadr TREE))))
    )
)

;;; If L is NIL, then return empty list because there is no possible sublist.
;;; Elif LEN is 0, then return empty list.
;;; Elif START is 0, then return the list of the car constructed with SUB-LIST(cdr)
;;; Else get SUB-LIST of cdr with a decremented start.

(defun SUB-LIST(L START LEN)
      ;; L is a list and LEN is the length of the sublist
      ;; returned, starting at index START
      (cond ((null L) NIL)
            ((= LEN 0) NIL)
            ((= START 0) (cons (car L) (SUB-LIST (cdr L) START (- LEN 1))))
            (t (SUB-LIST (cdr L) (- START 1) LEN))
      )
)

;;; If L is NIL, then return empty list because no split possible.
;;; Elif L is even length, then split directly in half using SUB-LIST.
;;; Else, split with first half being one element larger than second half.

(defun SPLIT-LIST(L)
      ;; L is a list
      ;; Returns the a list consisting of the two halves of L
      (cond ((null L) (list NIL NIL))
            ((evenp (length L)) (let
                                    ((split_pos (/ (length L) 2)))
                                    (list (SUB-LIST L 0 split_pos) (SUB-LIST L split_pos split_pos))
                                    )
            )
            (t (let
                  ((split_pos (/ (+ (length L) 1) 2)))
                  (list (SUB-LIST L 0 split_pos) (SUB-LIST L split_pos (- split_pos 1)))
                  )
            )
      )
)

;;; If TREE is NIL, then height is 0.
;;; Elif TREE is atom, leaf node is reached, and length of path is 0.
;;; Else, add 1 to the longest path of the tree rooted at each child.

(defun BTREE-HEIGHT(TREE)
      ;; TREE is a binary tree
      ;; Function returns the maximum path length from root to leaf node (height) of tree
      (cond ((null TREE) 0)
            ((atom TREE) 0)
            (t (let
                  ((LT (BTREE-HEIGHT (car TREE))) (RT (BTREE-HEIGHT (cadr TREE))))
                  (cond ((> LT RT) (+ 1 LT))
                        (t (+ 1 RT))
                  )
            ))
      )
)

;;; If LEAVES is NIL or of length 1 or 2, return LEAVES.
;;; Otherwise, split LEAVES and recursively call LIST2BTREE on each half and combine results.

(defun LIST2BTREE(LEAVES)
      ;; LEAVES is a list of atoms
      ;; Return a binary TREE such that LEAVES are the leaves
      (cond ((null LEAVES) NIL)
            ((= (length LEAVES) 1) (car LEAVES))
            ((= (length LEAVES) 2) LEAVES)
            (t (let
                  ((split (SPLIT-LIST LEAVES)))
                  (list (LIST2BTREE (car split)) (LIST2BTREE (cadr split)))
                  )
            )
      )
)

;;; If TREE is NIL, return NIL.
;;; Elif TREE is an atom, return a list of that atom.
;;; Else, return the appending of the BTREE2LIST of the two children.

(defun BTREE2LIST(TREE)
      ;; TREE is a binary tree
      ;; Returns list representation of atoms of leaves of tree
      (cond ((null TREE) NIL)
            ((atom TREE) (list TREE))
            (t (append (BTREE2LIST (car TREE)) (BTREE2LIST (cadr TREE))))
      )
)

;;; If both E1 E2 are null, then they are same.
;;; Elif only one is null, then they are not same.
;;; Elif both are atoms, result is if both atoms are equal.
;;; Elif only one is an atom, then they are not same
;;; Else, check if car and cdr are same.

(defun IS-SAME(E1 E2)
      ;; E1 and E2 are two LISP expressions
      ;; Returns if both represent same expression
      (cond ((and (null E1) (null E2)) t)
            ((null E1) NIL)
            ((null E2) NIL)
            ((and (atom E1) (atom E2)) (= E1 E2))
            ((atom E1) NIL)
            ((atom E2) NIL)
            (t (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2))))
      )
)

;;; If E2 is null, return E1.
;;; Elif E2 is an atom, then can simply append to E1 and return.
;;; Else, E2 is not an atom, so first FLATTEN-APPEND E1 with E2 first element, then again with E2 cdr.

(defun FLATTEN-APPEND(E1 E2)
      ;; E1 and E2 are two LISP expressions
      ;; Return E2 appended to E1 in left to right and depth first manner
      (cond ((null E2) E1)
            ((atom E2) (append E1 (list E2)))
            (t (FLATTEN-APPEND (FLATTEN-APPEND E1 (car E2)) (cdr E2)))
      )
)