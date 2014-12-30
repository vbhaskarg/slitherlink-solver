(defun check_side_num(i j num side)
  ;T -> 1 L -> 2 B -> 3 R - > 4
  ;     1
  ; 2  num  4
  ;     3
  (let ((row (array-dimension board 0))
        (col (array-dimension board 1))
        (num_row (+ i (* 2 (- side 2) (mod side 2))))
        (num_col (+ j (* 2 (- side 3) (mod (1+ side) 2)))))

    (cond ((is_valid_index num_row num_col)
           (return-from check_side_num (equal (digit-char-p (aref solver_board num_row num_col)) num))))))

(defun check_diag_num(i j num diag)
  ; Assigned diag values
  ; 2     1
  ;   num
  ; 4     3
  (let ((row (array-dimension board 0))
        (col (array-dimension board 1))
        (num_row (+ i (- (* 4 (floor (/ diag 3))) 2)))
        (num_col (+ j (- (* 4 (mod diag 2)) 2))))
    (cond ((is_valid_index num_row num_col)
           (return-from check_diag_num (equal (digit-char-p (aref solver_board num_row num_col)) num))))))

(defun check_corner(i j)
  (let ((row (array-dimension board 0))
        (col (array-dimension board 1))
        (result nil))
  ; This is what we return from here for each corner.
  ; (-1,-1)     (-1,1)
  ;
  ;
  ; (1,-1)      (1,1)
    (cond ((and (= i 1) (= j 1)) (setq result '(-1 -1)))
          ((and (= i 1) (= j (- col 2))) (setq result '(-1 1)))
          ((and (= i (- row 2)) (= j 1)) (setq result '(1 -1)))
          ((and (= i (- row 2)) (= j (- col 2))) (setq result '(1 1))))
    (return-from check_corner result)))

(defun strategy_0(i j)
  (let ((result nil))
    (setf (aref solver_board (1- i) j) #\X) 
    (setf (aref solver_board i (1- j)) #\X) 
    (setf (aref solver_board (1+ i) j) #\X) 
    (setf (aref solver_board i (1+ j)) #\X)

    (setq result (check_corner i j))
    (cond ((not (null result))
           (setf (aref solver_board (- i (* 2 (car result))) (+ j (cadr result))) #\X)
           (setf (aref solver_board (+ i (car result)) (- j (* 2 (cadr result)))) #\X)))

    ; Check if number 3 is in any of the adjacent cells
    ;    3
    ; 3  0  3
    ;    3
    (cond ((check_side_num i j 3 1) (setf (aref solver_board (- i 3) j) #\-) 
           (setf (aref solver_board (- i 2) (- j 1)) #\|) (setf (aref solver_board (- i 2) (+ j 1)) #\|)
           (setf (aref solver_board (- i 1) (- j 2)) #\-) (setf (aref solver_board (- i 1) (+ j 2)) #\-)))
    (cond ((check_side_num i j 3 2) (setf (aref solver_board i (- j 3)) #\|) 
           (setf (aref solver_board (- i 1) (- j 2)) #\-) (setf (aref solver_board (+ i 1) (- j 2)) #\-)
           (setf (aref solver_board (- i 2) (- j 1)) #\|) (setf (aref solver_board (+ i 2) (- j 1)) #\|)))
    (cond ((check_side_num i j 3 3) (setf (aref solver_board (+ i 3) j) #\-) 
           (setf (aref solver_board (+ i 2) (- j 1)) #\|) (setf (aref solver_board (+ i 2) (+ j 1)) #\|)
           (setf (aref solver_board (+ i 1) (- j 2)) #\-) (setf (aref solver_board (+ i 1) (+ j 2)) #\-)))
    (cond ((check_side_num i j 3 4) (setf (aref solver_board i (+ j 3)) #\|)
           (setf (aref solver_board (- i 1) (+ j 2)) #\-) (setf (aref solver_board (+ i 1) (+ j 2)) #\-)
           (setf (aref solver_board (- i 2) (+ j 1)) #\|) (setf (aref solver_board (+ i 2) (+ j 1)) #\|)))

    ; Check if number 3 is in any of the diagonally adjacent cells
    ; 3   3
    ;   0
    ; 3   3

    (cond ((check_diag_num i j 3 1) (setf (aref solver_board (- i 2) (+ j 1)) #\|)
           (setf (aref solver_board (- i 1) (+ j 2)) #\-)))
    (cond ((check_diag_num i j 3 2) (setf (aref solver_board (- i 2) (- j 1)) #\|)
           (setf (aref solver_board (- i 1) (- j 2)) #\-)))
    (cond ((check_diag_num i j 3 4) (setf (aref solver_board (+ i 2) (- j 1)) #\|)
           (setf (aref solver_board (+ i 1) (- j 2)) #\-)))
    (cond ((check_diag_num i j 3 3) (setf (aref solver_board (+ i 2) (+ j 1)) #\|)
           (setf (aref solver_board (+ i 1) (+ j 2)) #\-)))
  )
)

(defun strategy_1(i j)
  (let ((result nil))
    (setq result (check_corner i j))
    (cond ((not (null result))
           (setf (aref solver_board (+ i (car result)) j) #\X)
           (setf (aref solver_board i (+ j (cadr result))) #\X)))
  ))

(defun strategy_2(i j)
  (let ((result nil))
    (setq result (check_corner i j))
    (cond ((not (null result))
           (setf (aref  solver_board (+ i (car result)) (- j (* 2 (cadr result)))) #\-)
           (setf (aref  solver_board (- i (* 2 (car result))) (+ j (cadr result))) #\|)))
  ))

(defun strategy_3(i j)
  (let ((result nil))
    (setq result (check_corner i j))
    (cond ((not (null result))
           (setf (aref solver_board (+ i (car result)) j) #\-)
           (setf (aref solver_board i (+ j (cadr result))) #\|)))

    ; Check if number 3 is in any of the adjacent cells after the current cell
    ;  3  3
    ;  3
    (cond ((check_side_num i j 3 3) (setf (aref solver_board (- i 1) j) #\-)
           (setf (aref solver_board (+ i 1) j) #\-) (setf (aref solver_board (+ i 3) j) #\-)))
    (cond ((check_side_num i j 3 4) (setf (aref solver_board i (- j 1)) #\|)
           (setf (aref solver_board i (+ j 1)) #\|) (setf (aref solver_board i (+ j 3)) #\|)))

    ; Check if number 3 is in any of the diagonally adjacent cells after the current cell
    ;   3
    ; 3   3
    (cond ((check_diag_num i j 3 4)
           (setf (aref solver_board (- i 1) j) #\-) (setf (aref solver_board i (+ j 1)) #\|)
           (setf (aref solver_board (+ i 2) (- j 3)) #\|) (setf (aref solver_board (+ i 3) (- j 2)) #\-)))
    (cond ((check_diag_num i j 3 3)
           (setf (aref solver_board (- i 1) j) #\-) (setf (aref solver_board i (- j 1)) #\|)
           (setf (aref solver_board (+ i 2) (+ j 3)) #\|) (setf (aref solver_board (+ i 3) (+ j 2)) #\-)))
  )
)

(defun get_junction_edge (i j side)
  ; Side: 1 -> L 2 -> B 3 -> R 4 -> T
  (let ((row i) (col j)
        (edge_row 0) (edge_col 0))

    (setq edge_row (+ row (* (- 3 side) (mod (1- side) 2))))
    (setq edge_col (+ col (* (- side 2) (mod side 2))))
    (cond ((null (is_valid_index edge_row edge_col)) (return-from get_junction_edge nil))
          (T (return-from get_junction_edge (list edge_row edge_col))))))

(defun strategy_3_2(i j)
  (let ((row i) (col j)
        (edges_list nil) (temp_edge nil)
        (count 0) (j_row 0) (j_col 0)
        (j_side_1 0) (j_side_2 0)
        (j_side_3 0) (j_side_4 0)
        (side_3_edge nil) (side_4_edge nil))

    ; Check for possibilities of below types
    ; +   +   +
    ;   3
    ; +   + X +
    ;     X
    ; +   +   +
    ; diag order:
    ; <2>  <1>
    ;    3
    ; <4>  <3>
    (loop for diag from 1 to 4 do
      (setq count 0)
      (setq j_row (+ row (- (* 2 (floor (/ diag 3))) 1)))
      (setq j_col (+ col (- (* 2 (mod diag 2)) 1)))
      (cond ((= diag 1) (setq j_side_1 3))
            ((= diag 2) (setq j_side_1 4))
            ((= diag 3) (setq j_side_1 2))
            ((= diag 4) (setq j_side_1 1)))
      (setq j_side_2 (+ (mod j_side_1 4) 1))
      (setq j_side_3 (+ (mod j_side_2 4) 1))
      (setq j_side_4 (+ (mod j_side_3 4) 1))

      (setq side_3_edge (get_junction_edge j_row j_col j_side_3))
      (setq side_4_edge (get_junction_edge j_row j_col j_side_4))
    
      (cond ((or (equal (aref solver_board (car side_3_edge) (cadr side_3_edge)) #\Space)
                 (equal (aref solver_board (car side_4_edge) (cadr side_4_edge)) #\Space))
             (setq temp_edge (get_junction_edge j_row j_col j_side_1))
             (cond ((null temp_edge) (setq count (1+ count)))
                    (T (cond ((equal (aref solver_board (car temp_edge) (cadr temp_edge)) #\X) 
                              (setq count (1+ count))))))

             (setq temp_edge (get_junction_edge j_row j_col j_side_2))
             (cond ((null temp_edge) (setq count (1+ count)))
                    (T (cond ((equal (aref solver_board (car temp_edge) (cadr temp_edge)) #\X) 
                              (setq count (1+ count))))))
             (cond ((= count 2) 
                    (setq edges_list (list side_3_edge side_4_edge))
                    (apply_edges edges_list T)))))
    )))

(defun num_neutral(i j)
  (let ((edge_row 0) (edge_col 0) (edges_list nil))

    (loop for count from 1 to 4 do
      (setq edge_row (+ i (* (- 3 count) (mod (1- count) 2))))
      (setq edge_col (+ j (* (- count 2) (mod count 2))))
      (cond ((is_valid_index edge_row edge_col)
             (cond ((equal (aref solver_board edge_row edge_col) #\Space)
                    (setq edges_list (cons (list edge_row edge_col) edges_list)))))))
    (return-from num_neutral edges_list)))

(defun junction_neutral(i j)
  (let ((edge_row 0) (edge_col 0) (edges_list nil))

    (loop for count from 1 to 4 do
      (setq edge_row (+ i (* (- 3 count) (mod (1- count) 2))))
      (setq edge_col (+ j (* (- count 2) (mod count 2))))
      (cond ((is_valid_index edge_row edge_col)
             (cond ((equal (aref solver_board edge_row edge_col) #\Space)
                    (setq edges_list (cons (list edge_row edge_col) edges_list)))))))
    (return-from junction_neutral edges_list)))

(defun get_num_count (i j)
  (let ((total 0)
        (positive 0) (negative 0)
        (edge_row 0) (edge_col 0))

  (loop for count from 1 to 4 do
    (setq edge_row (+ i (* (- 3 count) (mod (1- count) 2))))
    (setq edge_col (+ j (* (- count 2) (mod count 2))))
    (cond ((is_valid_index edge_row edge_col)
           (cond ((equal (aref solver_board edge_row edge_col) #\Space)
                  (setq total (1+ total)))
                 ((equal (aref solver_board edge_row edge_col) #\X)
                  (setq total (+ total 1)) (setq negative (1+ negative)))
                 (T (setq total (+ total 1)) (setq positive (1+ positive)))))))
  (return-from get_num_count (list total positive negative))))

(defun get_junction_count (i j)
  (let ((total 0)
        (positive 0) (negative 0)
        (edge_row 0) (edge_col 0))

  (loop for count from 1 to 4 do
    (setq edge_row (+ i (* (- 3 count) (mod (1- count) 2))))
    (setq edge_col (+ j (* (- count 2) (mod count 2))))
    (cond ((is_valid_index edge_row edge_col)
           (cond ((equal (aref solver_board edge_row edge_col) #\Space)
                  (setq total (1+ total)))
                 ((equal (aref solver_board edge_row edge_col) #\X)
                  (setq total (1+ total)) (setq negative (1+ negative)))
                 (T (setq total (1+ total)) (setq positive (1+ positive)))))))
  (return-from get_junction_count (list total positive negative))))

(defun check_further(position)
  (let ((row (car position)) (col (cadr position)))
    (cond ((evenp row)
           (check_junction row (1- col)) (check_junction row (1+ col))
           (check_num (1- row) col) (check_num (1+ row) col)
           (check_num (1- row) (- col 2)) (check_num (1+ row) (- col 2))
           (check_num (1- row) (+ col 2)) (check_num (1+ row) (+ col 2)))
          (T
           (check_junction (1- row) col) (check_junction (1+ row) col)
           (check_num row (1- col)) (check_num row (1+ col))
           (check_num (- row 2) (1- col)) (check_num (- row 2) (1+ col))
           (check_num (+ row 2) (1- col)) (check_num (+ row 2) (1+ col))))))

(defun set_edge(edge edge_class)
  (let ((edge_char #\Space))
    (cond ((equal edge_class nil) (setq edge_char #\X))
          (T (cond ((evenp (car edge)) (setq edge_char #\-))
                   (T (setq edge_char #\|)))))
    (setf (aref solver_board (car edge) (cadr edge)) edge_char)
    ;(print_board solver_board)
  ))

(defun apply_edges (lst edge_class)
  (let ((edges_list lst))
    (loop for edge in edges_list do
      (setq level2_classification T)
      (set_edge edge edge_class)
      (check_further edge))))

(defun check_num (i j)
  (let ((row i) (col j) (num 0) (lst nil)
        (total_count 0) (positive_count 0) 
        (negative_count 0) (remaining 0) (needed 0))

    (cond ((not (is_valid_index row col))
           (return-from check_num)))
    (setq num (digit-char-p (aref solver_board row col)))
    (cond ((null num) (return-from check_num)))

    (setq lst (get_num_count row col))
    (setq total_count (car lst))
    (setq positive_count (cadr lst))
    (setq negative_count (caddr lst))
    (setq remaining (- total_count positive_count negative_count))
    (setq needed (- num positive_count))
    (cond ((and (> needed 0) (= remaining needed))
           (setq lst (num_neutral row col))
           (apply_edges lst T))
          ((and (= needed 0) (> remaining 0))
           (setq lst (num_neutral row col))
           (apply_edges lst nil))
          ((and (> needed 0) (= num 3)) (strategy_3_2 row col)))
  ))

(defun is_sub_loop (start_edge)
  (let* ((prev_edge nil)
         (curr_edge nil)
         (edge1 (get_next_edge solver_board (cons -1 start_edge)))
         (edge2 (get_next_edge solver_board (cons 1 start_edge))))

    (cond ((or (null edge1) (null edge2)) (return-from is_sub_loop nil))
          (T (setq curr_edge edge1)))
    (loop
      (cond ((equal start_edge (cdr curr_edge))
             (return-from is_sub_loop T))
            ((null curr_edge) (return-from is_sub_loop nil))
            (T (setq prev_edge curr_edge)))
      (setq curr_edge (get_next_edge solver_board prev_edge)))))

(defun check_sub_loops (i j)
  (let ((j_row i) (j_col j)
        (next_j_row 0) (next_j_col 0)
        (edge_row 0) (edge_col 0))

    (loop for j_side from 1 to 4 do
      (cond ((= j_side 1) (setq next_j_row j_row) (setq next_j_col (- j_col 2)))
            ((= j_side 2) (setq next_j_row (+ j_row 2)) (setq next_j_col j_col))
            ((= j_side 3) (setq next_j_row j_row) (setq next_j_col (+ j_col 2)))
            ((= j_side 4) (setq next_j_row (- j_row 2)) (setq next_j_col j_col)))
      (cond ((is_valid_index next_j_row next_j_col)
             (setq edge_row (/ (+ j_row next_j_row) 2))
             (setq edge_col (/ (+ j_col next_j_col) 2))
             (cond ((equal (aref solver_board edge_row edge_col) #\Space)
                     (cond ((check_juncture next_j_row next_j_col)
                            (set_edge (list edge_row edge_col) T)
                            (cond ((and (is_sub_loop (list edge_row edge_col)) (null (check_board solver_board)))
                                   (apply_edges (list (list edge_row edge_col)) nil))
                            (T
                             (setf (aref solver_board edge_row edge_col) #\Space)))))))
            )
      ))))

(defun check_junction (i j)
  (let ((row i) (col j) (lst nil)
        (total_count 0) (positive_count 0)
        (negative_count 0) (remaining 0))

    (setq lst (get_junction_count row col))
    (setq total_count (car lst))
    (setq positive_count (cadr lst))
    (setq negative_count (caddr lst))
    (setq remaining (- total_count positive_count negative_count))
    (cond ((and (= positive_count 1) (= remaining 1))
           (setq lst (junction_neutral row col))
           (apply_edges lst T))
          ((or (= positive_count 2) (and (= positive_count 0) (= remaining 1)))
           (setq lst (junction_neutral row col))
           (apply_edges lst nil)))
    (check_sub_loops row col)
  ))

(defun classify_board_level1()
  (let ((row (car (array-dimensions board)))
        (col (cadr (array-dimensions board))))
    (loop for i below row
      do (loop for j below col
           do (cond ((and (oddp i) (oddp j))
                     (let ((num (digit-char-p (aref solver_board i j))))
                       (cond ((not (null num))
                              (cond ((= num 0) (strategy_0 i j)))
                              (cond ((= num 1) (strategy_1 i j)))
                              (cond ((= num 2) (strategy_2 i j)))
                              (cond ((= num 3) (strategy_3 i j))))))))))
  ))

(defun classify_board_level2()
  (setq level2_classification nil)
  ; Second-level classification
  (let ((row (car (array-dimensions board)))
        (col (cadr (array-dimensions board))))

        (loop for i below row do
          (loop for j below col do
            (cond ((and (oddp i) (oddp j)) (check_num i j))
                   ((and (evenp i) (evenp j)) (check_junction i j)))))
  )
  (return-from classify_board_level2 level2_classification)
)

(defun pop_list(edge)
  (move edge)
  (setq direction_list (cdr direction_list)))

(defun push_list(edge direction)
  (move edge)
  (setq direction_list (cons direction direction_list)))

(defun check_juncture (row col)
  (let ((count 0))

  (cond ((check_edge board row (1- col)) (setq count (1+ count))))
  (cond ((check_edge board (1+ row) col) (setq count (1+ count))))
  (cond ((check_edge board row (1+ col)) (setq count (1+ count))))
  (cond ((check_edge board (1- row) col) (setq count (1+ count))))
  
  (cond ((<= count 1) (return-from check_juncture T))
        (T (return-from check_juncture nil)))))

(defun is_edge (this_board row col)
  (and (not (equal (aref this_board row col) #\Space))
       (not (equal (aref this_board row col) #\X))))

(defun num_bound_limit (num_row num_col prev)

  (cond ((not (is_valid_index num_row num_col)) (return-from num_bound_limit T)))
  (let ((num (digit-char-p (aref board num_row num_col)))
       (count 0))
    (cond ((null num) (return-from num_bound_limit T)))
    (cond ((is_edge board (1- num_row) num_col) (setq count (1+ count))))
    (cond ((is_edge board (1+ num_row) num_col) (setq count (1+ count))))
    (cond ((is_edge board num_row (1- num_col)) (setq count (1+ count))))
    (cond ((is_edge board num_row (1+ num_col)) (setq count (1+ count))))
    (cond ((equal prev T)
           (cond ((/= count num) (return-from num_bound_limit nil))
                 (T (return-from num_bound_limit T)))))
    (cond ((>= count num) (return-from num_bound_limit nil))
          (T (return-from num_bound_limit T)))))

(defun is_good_move (curr_edge_row curr_edge_col next_edge_row next_edge_col)
  (let ((num_3 nil)
        (curr_num_1 nil) (curr_num_2 nil)
        (next_num_1 nil) (next_num_2 nil))

    (cond ((evenp curr_edge_row)
           (setq curr_num_1 (list (1- curr_edge_row) curr_edge_col))
           (setq curr_num_2 (list (1+ curr_edge_row) curr_edge_col)))
          (T
           (setq curr_num_1 (list curr_edge_row (1- curr_edge_col)))
           (setq curr_num_2 (list curr_edge_row (1+ curr_edge_col)))))

    (cond ((evenp next_edge_row)
           (setq next_num_1 (list (1- next_edge_row) next_edge_col))
           (setq next_num_2 (list (1+ next_edge_row) next_edge_col)))
          (T 
           (setq next_num_1 (list next_edge_row (1- next_edge_col)))
           (setq next_num_2 (list next_edge_row (1+ next_edge_col)))))

    (cond ((is_valid_index (car curr_num_1) (cadr curr_num_1))
           (cond ((equal (digit-char-p (aref board (car curr_num_1) (cadr curr_num_1))) 3)
                  (setq num_3 curr_num_1)))))

    (cond ((is_valid_index (car curr_num_2) (cadr curr_num_2))
           (cond ((equal (digit-char-p (aref board (car curr_num_2) (cadr curr_num_2))) 3)
                  (setq num_3 curr_num_2)))))

    (cond ((not (null num_3))
           (cond ((null (num_bound_limit (car num_3) (cadr num_3) T))
                  (cond ((and (not (equal num_3 next_num_1)) 
                              (not (equal num_3 next_num_2))
                              (> (length edge_list) 2))
                         (return-from is_good_move nil)))))))

    (and (num_bound_limit (car next_num_1) (cadr next_num_1) nil) 
         (num_bound_limit (car next_num_2) (cadr next_num_2) nil))
))

(defun get_next(edge direction)
  ; This function returns in the below format:
  ; (a b (c d))
  ; a (1 to 4) the direction of the next edge at the prev juntion
  ;            If we get a positive edge we make it 4.
  ; b (+1 or -1) search direction of the next edge
  ; (c d) next edge
  (let ((next_edge nil) (j_side 0) (next_edge_direction -1)
        (next_edge_col 0) (next_edge_col 0)
        (new_row 0) (new_col 0) (j_row 0) (j_col 0)
        (row (car (array-dimensions board)))
        (col (cadr (array-dimensions board))))

  (cond ((equal (car direction) 4) (return-from get_next nil)))
  (setq next_edge (get_next_edge solver_board (cons (cadr direction) edge)))
  (cond ((not (null next_edge))
;         (format t "~%I found something interesting: ~S~%" next_edge)
         (return-from get_next (list 4 (car next_edge) (cdr next_edge)))))
  
  (cond ((evenp (car edge))
         (setq j_row (car edge))
         (setq j_col (+ (cadr edge) (cadr direction)))
         (setq prev_j_side (- 2 (cadr direction))))
        (T
         (setq j_row (+ (car edge) (cadr direction)))
         (setq j_col (cadr edge))
         (setq prev_j_side (+ 3 (cadr direction)))))

  (setq j_side (car direction))
  (loop while (< j_side 4) do
    (setq j_side (1+ j_side))
    (cond ((/= j_side prev_j_side)
           (cond ((= j_side 1) (setq next_edge_direction -1)
                  (setq new_row j_row) (setq new_col (- j_col 2))) 
                 ((= j_side 2) (setq next_edge_direction 1)
                  (setq new_row (+ j_row 2)) (setq new_col j_col))
                 ((= j_side 3) (setq next_edge_direction 1)
                  (setq new_row j_row) (setq new_col (+ j_col 2)))
                 ((= j_side 4) (setq next_edge_direction -1)
                  (setq new_row (- j_row 2)) (setq new_col j_col)))
    
           (setq next_edge_row (/ (+ j_row new_row) 2))
           (setq next_edge_col (/ (+ j_col new_col) 2))
           (cond ((and (>= new_row 0) (>= new_col 0) (< new_row row) (< new_col col))
                  (cond ((not (equal (aref solver_board next_edge_row next_edge_col) #\X))
                         (cond ((and (check_juncture new_row new_col)
                                     (is_good_move (car edge) (cadr edge) next_edge_row next_edge_col))
                                (setq next_edge (list next_edge_row next_edge_col))
                                (return-from get_next (list j_side next_edge_direction next_edge))))))
                 )))))
  (return-from get_next (list 0 0 nil))))

(defun start_search(start_edge)
  (let ((curr_edge nil) (next_edge nil)
       (result nil) (direction nil)
       (start_direction -1) (counter 1))
  
  (cond ((not (null (get_next_edge solver_board (cons -1 start_edge)))) (setq start_direction -1))
        ((not (null (get_next_edge solver_board (cons 1 start_edge)))) (setq start_direction 1)))
  (push_list start_edge (list 0 start_direction))

  (loop while (not (null edge_list)) do
    (setq curr_edge (car edge_list))
    (setq direction (car direction_list))
    (setq result (get_next curr_edge direction))
    (setq next_edge (caddr result))
    ;(format t "~%Result: ~S Next edge: ~S~%" result next_edge)
    (cond ((null next_edge)
           (pop_list curr_edge))
          ((equal next_edge start_edge)
           ;(format t "~%Checking if we reached the goal~%")
           (cond ((check_board board) (return-from start_search T))
                 (T (pop_list curr_edge))))
          (T
           (setq direction (cons (car result) (cdr direction)))
           (setq direction_list (cons direction (cdr direction_list)))
           (setq direction (list 0 (cadr result)))
           (push_list next_edge direction)))
   ; (setq counter (1+ counter))
    (cond ((= (mod counter 10000) 0)
           (format t "~%Edge list: ~S~%" edge_list)
           (format t "~%Direction list: ~S~%" direction_list)
           (print_board board)))
  )
  (return-from start_search nil)))

(defun get_start_edge()
  (let ((count 0) (edge_row 0) (edge_col 0)
        (i 1) (j 1)
        (row (car (array-dimensions board)))
        (col (cadr (array-dimensions board)))
        (before nil) (after nil) (temp nil)
        (max_edges 0) (start_edge nil))

    (cond ((equal positive_start_edge nil)
           ; Pick maximum connected edge in the classified board.
           (loop for i below row do
             (loop for j below col do
               (cond ((or (and (evenp i) (oddp j)) (and (oddp i) (evenp j)))
                      (cond ((is_edge solver_board i j)
                             (setq count 1)
                             (setq before (get_next_edge solver_board (list -1 i j)))
                             (setq after (get_next_edge solver_board (list 1 i j)))
                             (setq positive_start_edge T)
                             (cond ((not (null before)) (setq temp before)))
                             (cond ((not (null after))
                                    (cond ((not (null temp)) (setq temp nil))
                                          (T (setq temp after)))))
                             (loop while (not (null temp)) do
                               (setq count (1+ count))
                               (setq temp (get_next_edge solver_board temp))
                             )
                             (cond ((>= count max_edges) (setq max_edges count) (setq start_edge (list i j))))
                             (setq before nil)
                             (setq after nil)
                             (setq temp nil)
                            ))))))
           (cond ((equal positive_start_edge T) (return-from get_start_edge start_edge)))))

    ; If positive edge is true and we still get to this point,
    ; then there is something wrong with the code
    (cond ((equal positive_start_edge T) (return-from get_start_edge nil)))
    
    ; If start cell is nil, then traverse the board and
    ; return the left edge of the first non-empty cell.
    (loop while (null start_position) do
      (cond ((not (equal (aref solver_board i j) #\Space))
             (setq start_position (list 0 i j))))
      (setq j (+ j 2))
      (cond ((>= j col) (setq i (+ i 2)) (setq j 1)))
      (cond ((>= i row) (return-from get_start_edge nil))))

    (setq count (car start_position))
    (cond ((>= count 4) (return-from get_start_edge nil)))

    (setq count (1+ count))
    (setq start_position (cons count (cdr start_position)))
    ; count: 1 -> Left, 2 -> Bottom, 3 -> Right, 4-> TOP
    (setq row (+ (cadr start_position) (* (- 3 count) (mod (1- count) 2))))
    (setq col (+ (caddr start_position) (* (- count 2) (mod count 2))))
    (return-from get_start_edge (list row col))))

(defun start_solver()
  (let ((result nil) (start_edge nil))
    (setq solver_board (copy-array board))
    (cond ((> (array-dimension board 0) 7) 
           (classify_board_level1)
           (format t "~%Level-1 classification:~%")
           (print_board solver_board)
           (loop while (classify_board_level2))
           (format t "~%Level-2 classification:~%")
           (print_board solver_board)))

    (loop while T do
      (setq start_edge (get_start_edge))
      (format t "Start edge: ~S~%" start_edge)
      (cond ((null start_edge) (return)))
      (setf (aref solver_board (car start_edge) (cadr start_edge)) #\-)
      (cond ((not (null (start_search start_edge))) (setq result T) (return))
            (T (setf (aref solver_board (car start_edge) (cadr start_edge)) #\Space))))

    (cond ((null result) (format t "~% Couldn't find solution for this board. ~%"))
          (T (format t "~%Yay! Got the solution:")
             (print_board board)
             (format t "~%List of moves:~%")
             (format t "(Consider board layout as an array consisting of '+' signs, edges, and numbers)~%")
             (format t "~%~S~%" (reverse edge_list))))))

(defun auto_solve()
  (reset_board)
  (let ((real1 (get-internal-real-time))
        (run1 (get-internal-run-time)))
    (start_solver)
    (let ((run2 (get-internal-run-time))
          (real2 (get-internal-real-time)))
      (format t "~%Solver took:~%")
      (format t "  ~f seconds of real time~%"
        (/ (- real2 real1) internal-time-units-per-second))
      (format t "  ~f seconds of run time~%"
        (/ (- run2 run1) internal-time-units-per-second)))))
