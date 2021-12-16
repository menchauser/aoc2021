(defun read-input (path)
  (let ((lines (uiop:read-file-lines path)))
    (make-array (list (length lines) (length (car lines)))
                :initial-contents
                (mapcar (lambda (line)
                          (mapcar (lambda (c) (- (char-int c) (char-int #\0)))
                                  (coerce line 'list)))
                        lines))))

(defun print-array (arr)
  (loop for r from 0 below (array-dimension arr 0) do
    (loop for c from 0 below (array-dimension arr 1) do
      (format t "~4d" (aref arr r c)))
    (format t "~%")))

(defun neighbours (cave row col)
  (let ((rows (array-dimension cave 0))
        (cols (array-dimension cave 1)))
    ;; final point has no neighbours
    (if (equal (cons row col) (cons (1- rows) (1- cols)))
        nil
        (remove-if
         (lambda (x) (or
                      (< (car x) 0)         ; over the top row
                      (>= (car x) rows)     ; over the bottom row
                      (< (cdr x) 0)         ; over the left col
                      (>= (cdr x) cols)     ; over the right col
                      ;; there is no point to go left on edge rows
                      (and (< (cdr x) col)
                           (or (= (car x) 0) (= (car x) (1- cols))))
                      ;; there is no point to go up on the edge cols
                      (and (< (car x) row)
                           (or (= (cdr x) 0) (= (cdr x) (1- cols))))))
         (list (cons (1- row) col)
               (cons row (1+ col))
               (cons (1+ row) col)
               (cons row (1- col)))))))

(defun min-by-distance (queue distances)
  (when queue
    (loop with min-pos = (car queue)
          for pos in (cdr queue)
          when (< (aref distances (car pos) (cdr pos))
                  (aref distances (car min-pos) (cdr min-pos)))
            do (setf min-pos pos)
          finally (return min-pos))))
    
(defun shortest-path-dijkstra (cave)
  "Find shortest path from top-left to bottom-right corner. Current position is
encoded as POS cons cell."
  (let ((max-row (1- (array-dimension cave 0)))
        (max-col (1- (array-dimension cave 1)))
        (queue nil)
        (distances (make-array (array-dimensions cave))))
    (loop for row from 0 to max-row do
      (loop for col from 0 to max-col
            do (setf (aref distances row col) most-positive-fixnum)
               (push (cons row col) queue)))
    (setf (aref distances 0 0) 0)
    (setf queue (sort queue (lambda (x y)
                              (< (aref distances (car x) (cdr x))
                                 (arшef distances (car y) (cdr y))))))
    (loop for n = (pop queue)
          while n
          do (loop for neighbour in (neighbours cave (car n) (cdr n))
                   for new-length = (+ (aref distances (car n) (cdr n))
                                       (aref cave (car neighbour) (cdr neighbour)))
                   for old-length = (aref distances (car neighbour) (cdr neighbour))
                   when (< new-length old-length)
                     do (setf (aref distances (car neighbour) (cdr neighbour))
                              new-length)))
             (setf queue (sort queue (lambda (x y)
                                       (< (aref distances (car x) (cdr x))
                                          (aref distances (car y) (cdr y))))))
    (aref distances max-row max-col)))

(defun part1 (path)
  (let ((cave (read-input path)))
    (shortest-path-dijkstra cave)))

(defun part2 (path)
  (let* ((cave (read-input path))
         (rows (array-dimension cave 0))
         (cols (array-dimension cave 1))
         (new-dims (list (* 5 rows) (* 5 cols)))
         (big-cave (make-array new-dims)))
    (loop for i from 0 to 4 do
      (loop for j from 0 to 4 do
        (loop for row from 0 below rows do
          (loop for col from 0 below cols do
            (setf (aref big-cave (+ (* i rows) row) (+ (* j cols) col))
                  (1+ (mod (1- (+ (aref cave row col) i j)) 9)))))))
    (shortest-path-dijkstra big-cave)))
