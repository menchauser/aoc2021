;; Let's store input as list of numbers and each board as list of numbers

(defmacro bracify (s)
  `(concatenate 'string "(" ,s ")"))

(defun read-input (line)
  "Read line with comma-separated numbers into list of numbers."
  (read-from-string (bracify (substitute #\Space #\, line))))

(defun read-board (lines)
  "Reads board from first five of input LINES into list of lists of numbers."
  (let* ((board-lines (subseq lines 0 5))
         (number-lines
           (mapcar (lambda (l) (read-from-string (bracify l))) board-lines)))
    number-lines))

(defun read-boards (lines)
  "Reads list of string board representations into list of boards."
  ;; every time we encounter empty line - we try reading next five lines in board
  (labels ((read-boards-impl (lines result)
             (cond
               ((null lines) (reverse result))
               ((and (equal "" (car lines)) (consp (cdr lines)))
                (read-boards-impl
                 (subseq lines 6)
                 (cons (read-board (cdr lines)) result)))
               (t (error "Unexpected case. Line: ~S" (car lines))))))
    (read-boards-impl lines nil)))           

(defmacro all-nulls (xs)
  `(every #'null ,xs))

(defun won-by-rowsp (board)
  "Check if board has any winner row: all filled with NILs."
  (cond
    ((null board) nil)
    ((all-nulls (car board)) t)
    (t (won-by-rowsp (cdr board)))))

(defun won-by-collsp (board)
  "Check if board has any winner column: all filled with NILs."
  (let ((col-count (length (car board))))
    (dotimes (i col-count)
      (let ((col (mapcar (lambda (row) (elt row i)) board)))
        (when (all-nulls col)
          (return t))))))

(defun won-by-collsp-2 (board max-count)
  ;;(format t "Max count: ~a, board:~%" max-count)
  ;;(print-board board)
  (cond
    ((equal max-count 0) nil)
    ((all-nulls (mapcar #'car board)) t)
    (t (won-by-collsp-2 (mapcar #'cdr board) (1- max-count)))))

(defun wonp (board)
  (or (won-by-rowsp board)
      (won-by-collsp board)))
  
(defun find-winner-board (boards)
  "Check if any board is in winner state and return it. Returns NIL if no such 
board."
  (find-if #'wonp boards))

(defun calculate-score (board num)
  (* num
     (reduce
      #'+
      (mapcar
       (lambda (row) (reduce #'+ (remove nil row)))
       board))))

(defun print-board (board)
  (dolist (row board)
    (format t "~{~3d~^ ~}~%" row)))

(defun part1 (path)
  (let* ((lines (uiop:read-file-lines path))
         (input (read-input (car lines)))
         (boards (read-boards (cdr lines))))
    ;; now we play the game:
    ;;   loop over input
    ;;   replace every found input item with NIL in boards
    ;;   check if any board won
    ;;   calculate score
    (dolist (n input)
      (nsubst nil n boards)
      (let* ((winner-board (find-winner-board boards)))
        (when winner-board
          (return (calculate-score winner-board n)))))))

(defun part2 (path)
  (let* ((lines (uiop:read-file-lines path))
         (input (read-input (car lines)))
         (boards (read-boards (cdr lines))))
    (dolist (n input)
      (nsubst nil n boards)
      ;; if only one board remains and it is won by input - we stop
      ;; otherwise: make remove winner boards and continue
      (if (and (equal (length boards) 1) (wonp (car boards)))
          (return (calculate-score (car boards) n))
          (setf boards (delete-if #'wonp boards))))))

