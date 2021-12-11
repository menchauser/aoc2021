(defun read-input (path)
  (let ((lines (uiop:read-file-lines path)))
    (make-array (list (length lines) (length (car lines)))
                :element-type 'fixnum
                :initial-contents
                (mapcar
                 (lambda (line)
                   (mapcar
                    (lambda (c) (- (char-int c) (char-int #\0)))
                    (coerce line 'list)))
                 lines))))

(defun nstep (grid)
  ;; first we increase energy level of each octopus
  (let ((rows (array-dimension grid 0))
        (cols (array-dimension grid 1)))
    (loop for i from 0 below rows do
      (loop for j from 0 below cols do
        (incf (aref grid i j))))
    ;; flash each octopus with level > 9
    ;; replace flashed octopus with -1
    ;; (format t "~a~%" grid)
    (loop
      for flash-coords = (loop named outer for i from 0 below rows do
        (loop for j from 0 below cols do
          (when (> (aref grid i j) 9)
            (return-from outer (cons i j)))))
      while flash-coords
      for row = (car flash-coords)
      for col = (cdr flash-coords)
      do (progn 
           ;; we found next flash position
           ;; from it we increase energy level for adjacent positions
           ;; we increase energy for flash position but later we will reset it to -1
           (loop for i from (max (1- row) 0) to (min (1+ row) (1- rows)) do
             (loop for j from (max (1- col) 0) to (min (1+ col) (1- cols))
                   when (>= (aref grid i j) 0)
                     do (incf (aref grid i j))))
           ;; after we flash adjacent positions: we set flashed octopus to -1
           (setf (aref grid row col) -1)))
    ;; in the end reset all flashed octopus (> 9) to 0 and count number of flashes
    (loop for i from 0 below rows sum
      (loop for j from 0 below cols
            counting (minusp (aref grid i j)) into flashes
            when (minusp (aref grid i j))
              do (setf (aref grid i j) 0)
            finally (return flashes)))))

(defun part1 (path)
  (let ((grid (read-input path)))
    (loop for n from 1 to 100
          sum (nstep grid))))

(defun part2 (path)
  (let* ((grid (read-input path))
         (rows (array-dimension grid 0))
         (cols (array-dimension grid 1)))
    (loop
      ;; the upper bound is set just for the safety
      for n from 1 to 1000
      do (progn
           (nstep grid)
           (let ((all-flashed
                   (loop named outer for i from 0 below rows
                         do (loop for j from 0 below cols 
                                  when (> (aref grid i j) 0)
                                    do (return-from outer nil))
                         finally (return-from outer t))))
             (when all-flashed
               (format t "All octopuses flashed on step ~a~%" n)
               (return)))))))

(defun print-grid (grid)
  (loop for i from 1 below (array-dimension grid 0) do
    (loop for j from 1 below (array-dimension grid 1) do
      (let ((x (aref grid i j)))
        (if (= x 0)
            (format t "~c[1m0~c[0m" #\ESC #\ESC)
            (format t "~c[2m~a~c[0m" #\ESC x #\ESC)))
      finally (format t "~%"))))
