(defun print_game_text()
  (format t "~%~%Description:~%
    The objective of the game is to find a single looped path with no crosses or branches.
    The numbers indicate how many lines surround it.~%
Instructions:~%
    Each move will be specified as a triple -
    N N L - The first N represents a row on the board, second N represents a column in that row
    and L represents the line to be set - T for top, B for bottom, L for left, and R for right.~%
    Example: 2 1 T - to set second row, first column's top line.~%
    No other punctuation or characters allowed.~%
Other commands supported:~%
    solve - To auto solve the board
    reset - To reset the board
    new - To start a new game
    quit - To quit the current game~%
All commands are case sensitive.~%~%"))

(defun copy-array (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

(defun is_valid_index (i j)
  (and (>= i 0) (>= j 0)
       (< i (array-dimension board 0))    
       (< j (array-dimension board 1))))

(defun print_board(this_board)
  (format t "~%~%")
  (format t "     ")
  (loop for j below (cadr (array-dimensions this_board))
    do (cond ((oddp j) 
              (cond ((< j 17) (format t "~D " (/ (+ 1 j) 2)))
                    (T (format t "~D" (/ (+ 1 j) 2)))))
             (T (format t "  "))))
  (format t "~%")
  (loop for i below (car (array-dimensions this_board))
    do (cond ((oddp i)
              (cond ((< i 19) (format t "  ~D  " (/ (+ 1 i) 2)))
                    (T (format t " ~D  " (/ (+ 1 i) 2)))))
             (T (format t "     ")))
       (loop for j below (cadr (array-dimensions this_board))
         do (format t "~a " (aref this_board i j)))
       (format t "~%"))
  (format t "~%~%"))

(defun reset_board()
  (let ((row (car (array-dimensions board)))
        (col (cadr (array-dimensions board))))
    (loop for i below row
      do (loop for j below col
           do (cond ((or (and (oddp i) (evenp j)) (and (evenp i) (oddp j))) 
                     (setf (aref board i j) #\Space))))))
    (setq edge_list nil)
    (setq positive_start_edge nil))

; Read the provided input board file
(defun get_config()
  (format t "~%Enter the board file name: ")
  (let ((board_file (read-line)))
    (loop while (null (probe-file board_file))
      do (format t "~%Error: ~A not present!~%" board_file)
         (format t "~%Please enter another file name: ")
         (setq board_file (read-line)))

    (with-open-file (stream board_file)
      (loop for line = (read-line stream nil)
        while line
        collect line))))

(defun board_init()
  (let* 
    ; Read config from input file and place it in an array
    ((config_list (get_config))
     (config (make-array (list (length config_list)
                               (length (first config_list)))
                         :initial-contents config_list))

     ; Build a board with the board config
     (row (+ 1 (* 2 (car (array-dimensions config)))))
     (col (+ 1 (* 2 (cadr (array-dimensions config))))))

    (setq board (make-array (list row col) 
                        :element-type 'character
                        :initial-element #\Space))

    (loop for i below row
      do (loop for j below col
           do (cond ((and (evenp i) (evenp j))
                     (setf (aref board i j) #\+))
                    ((and (oddp i) (oddp j))
                     (setf (aref board i j) 
                     (aref config (/ (- i 1) 2) (/ (- j 1) 2))))))))
    (format t "~%~%****New game started****~%~%")
    (setq temparr (copy-array board))
    (print_board board))

(defun check_bound(this_board num_row num_col)
  (let ((num (digit-char-p (aref this_board num_row num_col)))
        (count 0))
    (cond ((not (equal (aref this_board (1- num_row) num_col) #\Space)) (setq count (1+ count))))
    (cond ((not (equal (aref this_board (1+ num_row) num_col) #\Space)) (setq count (1+ count))))
    (cond ((not (equal (aref this_board num_row (1- num_col)) #\Space)) (setq count (1+ count))))
    (cond ((not (equal (aref this_board num_row (1+ num_col)) #\Space)) (setq count (1+ count))))
    (cond ((= num count) (return-from check_bound T))
          (T (return-from check_bound nil)))))

(defun check_num_bounds(this_board)
  (let ((row (car (array-dimensions this_board)))
        (col (cadr (array-dimensions this_board))))
    (loop for i below row
      do (loop for j below col
           do (cond ((and (oddp i) (oddp j))
                     (cond ((not (equal (aref this_board i j) #\Space)) 
                            (cond ((null (check_bound this_board i j))
                                   (return-from check_num_bounds nil)))))))))
    (return-from check_num_bounds T)))

; Check if an edge is present at the specified position.
(defun check_edge(this_board edge_row edge_col)
  (let ((row (car (array-dimensions this_board)))
        (col (cadr (array-dimensions this_board))))
    (cond ((and (>= edge_row 0) (>= edge_col 0) (< edge_row row) (< edge_col col))
           (if (and (not (equal (aref this_board edge_row edge_col) #\Space))
                    (not (equal (aref this_board edge_row edge_col) #\X)))
               (return-from check_edge T)))
          (T (return-from check_edge nil)))))

; Return next edge while checking for a loop.
(defun get_next_edge(this_board edge)
  (let
    ; Horizontal edge: +1 and vertical edge: -1
    ; Set default to horizontal 
    ((edge_type 1)
     (count 0)
     (edge_row (cadr edge))
     (edge_col (caddr edge))
     (next_edge nil))

    (cond ((oddp (cadr edge)) (setq edge_type -1)))
    (cond ((= (car edge) -1)
           (cond ((check_edge this_board (1- edge_row) (1- edge_col)) (1+ count)
                  (setq next_edge (list -1 (1- edge_row) (1- edge_col)))))
           (cond ((check_edge this_board (+ edge_row (* 1 edge_type)) (- edge_col (* 1 edge_type))) (1+ count)
                  (setq next_edge (list 1 (+ edge_row (* 1 edge_type)) (- edge_col (* 1 edge_type))))))
           (cond ((check_edge this_board (- edge_row (- 1 edge_type)) (- edge_col (+ 1 edge_type))) (1+ count)
                  (setq next_edge (list -1 (- edge_row (- 1 edge_type)) (- edge_col (+ 1 edge_type)))))))
          (T
           (cond ((check_edge this_board (1+ edge_row) (1+ edge_col)) (1+ count)
                  (setq next_edge (list 1 (1+ edge_row) (1+ edge_col)))))
           (cond ((check_edge this_board (- edge_row (* 1 edge_type)) (+ edge_col (* 1 edge_type))) (1+ count)
                  (setq next_edge (list -1 (- edge_row (* 1 edge_type)) (+ edge_col (* 1 edge_type))))))
           (cond ((check_edge this_board (+ edge_row (- 1 edge_type)) (+ edge_col (+ 1 edge_type))) (1+ count)
                  (setq next_edge (list 1 (+ edge_row (- 1 edge_type)) (+ edge_col (+ 1 edge_type))))))))
    
    (if (> count 1) (setq next_edge nil)) 
    (return-from get_next_edge next_edge)))

; Check if the board has reached a goal state.
(defun check_board(this_board)
  (cond ((null edge_list) (return-from check_board nil)))
  (let* ((temp_list edge_list)
         (start_edge (car edge_list))
         (prev_edge nil)
         (curr_edge nil)
         (closed_loop nil)
         (edge_list_empty nil)
         (edge1 (get_next_edge this_board (cons -1 start_edge)))
         (edge2 (get_next_edge this_board (cons 1 start_edge))))
        
    (setq temp_list (remove start_edge temp_list :test 'equal))
    (cond ((or (null edge1) (null edge2)) (return-from check_board nil))
          (T (setq curr_edge edge1)))
    (loop
      (cond ((equal start_edge (cdr curr_edge))
             (setq closed_loop T)
             (if (null temp_list) (setq edge_list_empty T))
             (return))
            ((null curr_edge) (return))
            (T (setq prev_edge curr_edge)
               (setq temp_list (remove (cdr curr_edge) temp_list :test 'equal))))
      (setq curr_edge (get_next_edge this_board prev_edge)))

    (cond ((and closed_loop edge_list_empty)
           (return-from check_board (check_num_bounds this_board)))
          (T (return-from check_board nil)))))

(defun print_invalid()
  (format t "~%~%Invalid. Try again.~%~%"))

(defun validate_move(move)
  (cond ((null move) (return-from validate_move nil)))
  (let ((edge_row (car move))
        (edge_col (cadr move))
        (row (/ (- (car (array-dimensions board)) 1) 2))
        (col (/ (- (cadr (array-dimensions board)) 1) 2)))
    (cond ((integerp edge_row) 
           (cond ((not (and (> edge_row 0) (<= edge_row row))) 
                  (return-from validate_move nil))))
          (T (return-from validate_move nil)))
    (cond ((integerp edge_col) 
           (cond ((not (and (> edge_col 0) (<= edge_col col))) 
                  (return-from validate_move nil))))
          (T (return-from validate_move nil)))
    (return-from validate_move T)))

(defun move(move)
  (let ((edge_char #\Space) (row (car move)) (col (cadr move)))
    (cond ((evenp row) (setq edge_char #\-))
          (T (setq edge_char #\|)))

    (cond ((equal (aref board row col) #\Space)
           (setf (aref board row col) edge_char)
           (setq edge_list (cons move edge_list)))
          (T (setf (aref board row col) #\Space) 
             (setq edge_list (remove move edge_list :test 'equal))))))
	
(defun make_move(move)
  (cond ((not (validate_move move))
         (print_invalid)
         (return-from make_move nil)))
  (let ((edge_type 1)
        (row (* (car move) 2))
        (col (* (cadr move) 2))
        (edge (caddr move)))
    (cond ((equal edge #\T) (setq row (- row 2)) (setq col (- col 1)))
          ((equal edge #\B) (setq col (- col 1)))
          ((equal edge #\L) (setq row (- row 1)) (setq col (- col 2)))
          ((equal edge #\R) (setq row (- row 1)))
          (T (print_invalid) (return-from make_move nil)))

    (move (list row col))
    (print_board board)
    (return-from make_move (check_board board))))

(defun parse_input(input_str)
  (let ((count 0) (move nil))
    (with-input-from-string (stream input_str)
      (cond ((listen stream) (setq move (cons (read stream) move)))
            (T (return-from parse_input nil)))
      (cond ((listen stream) (setq move (cons (read stream) move)))
            (T (return-from parse_input nil)))
      (cond ((listen stream) (setq move (cons (read-char stream) move)))
            (T (return-from parse_input nil)))
      (cond ((listen stream) (setq move nil))))
    (return-from parse_input (reverse move))))

(defun start_new_game()
  (setq board nil)
  (setq edge_list nil)
  (setq start_position nil)
  (setq direction_list nil)
  (board_init))

(defun query_new_game()
  (loop while T do
    (format t "Start new game? (y/n):")
      (let ((new_game (read-line)))
        (cond ((equal new_game "n")
               (format t "~%Quitting the game.~%~%")
               (return-from query_new_game nil))
              ((equal new_game "y")
               (return-from query_new_game T))
              (T (print_invalid))))))

(defun start_game()
  (let ((continue T) (win nil))
    (loop
      (format t "~%Enter next move: ")
      (setq move (read-line))
      (cond ((equal move "quit") (setq continue nil))
            ((equal move "reset")
             (reset_board)
             (format t "~%****Board reset done****~%")
             (print_board board))
            ((equal move "solve") (auto_solve) (setq win T))
            ((equal move "new")
             (start_new_game))
            (T (setq win (make_move (parse_input move)))))
      (cond ((equal win T)
             (format t "~%~%Congratulations! You won the game.~%~%")
             (cond ((query_new_game) (setq win nil) (start_new_game))
                   (T (setq continue nil)))))
      (when (not continue) (return)))))

(defun slither()
  (print_game_text)
  (board_init)
  (start_game))
