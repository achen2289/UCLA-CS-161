;;; Alex Chen
;;; Spring '22

;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
  (solver nil n delta)
)

;; Takes in a solution of variable assignments, number of variables in cnf, and a cnf expression
;; Returns one solution to the CNF, or nil if none
;; Starts with current variable being assigned, then tries to apply positive and negative values to arrive at next CNF states
;; If cnf is equal to 'invalid, then no solution exists
(defun solver (solu n cnf)
  (cond ((equal cnf 'invalid) NIL)
        ((null cnf) (finalize_solu solu n))
        (t (let* (
              (next_var_pos (+ 1 (length solu)))
              (next_var_neg (- next_var_pos))
              (next_cnf_pos (get_next_cnf next_var_pos cnf))
              (next_cnf_neg (get_next_cnf next_var_neg cnf))
           )(
              or (solver (cons next_var_pos solu) n next_cnf_pos)
                 (solver (cons next_var_neg solu) n next_cnf_neg)
           )
        ))
  )
)

;; solu contains variable assignments up to a certain value <= n
;; Add rest of assignments (positive literals) up to n if needed
(defun finalize_solu (solu n)
  (cond ((= (length solu) n) solu)
        (t (finalize_solu (cons (+ 1 (length solu)) solu) n))
  )
)

;; Take in a literal and cnf and return the next cnf after applying literal to cnf
;; Return 'invalid if not a valid assignment
(defun get_next_cnf (literal cnf)
  (cond ((valid_assign literal cnf) (reduce_cnf literal cnf))
        (t 'invalid)
  )
)

;; Return if literal can be assigned and not violate any constraints within cnf
;; If any of the clauses within cnf are the negative literal, then this is violating a constraint
;; Essentially backtracking - prune tree at any node that has no more solutions
(defun valid_assign (literal cnf)
  (cond ((null cnf) t)
        ((equal (list (- literal)) (car cnf)) nil)
        ((valid_assign literal (cdr cnf)))
  )
)

;; Reduce cnf by removing literal from any clause in cnf
;; If literal is not in a clause, keep that clause but remove the negative literal
;; Essentially forward checking - if the literal satisfies a clause, remove clause and all other variables in that clause
(defun reduce_cnf (literal cnf)
  (cond ((null cnf) nil)
        ((> (count literal (car cnf)) 0) (reduce_cnf literal (cdr cnf)))
        (t (cons (remove (- literal) (car cnf)) (reduce_cnf literal (cdr cnf))))
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

