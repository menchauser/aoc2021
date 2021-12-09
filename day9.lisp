(defun read-input (path)
  (let* ((lines (uiop:read-file-lines path))
         (rows (length lines))
         (cols (length (car lines))))
    (make-array (list rows cols)
                :element-type 'fixnum
                :initial-contents
                (mapcar (lambda (line)
                          (mapcar (lambda (c) (- (char-int c) (char-int #\0)))
                                  (coerce line 'list)))
                        lines))))

(defun is-low-point (data i j)
  "Check if element at row I and column J is a low point."
  (let ((x (aref data i j))
        (max-row (1- (array-dimension data 0)))
        (max-col (1- (array-dimension data 1))))
    (and
     ;; top
     (if (= i 0) t
         (< x (aref data (1- i) j)))
     ;; left
     (if (= j 0) t
         (< x (aref data i (1- j))))
     ;; bottom
     (if (= i max-row) t
         (< x (aref data (1+ i) j)))
     ;; right
     (if (= j max-col) t
         (< x (aref data i (1+ j)))))))

(defun part1 (path)
  (let ((data (read-input path)))
    (loop for i from 0 below (array-dimension data 0) sum
      (loop for j from 0 below (array-dimension data 1)
            when (is-low-point data i j)
              sum (1+ (aref data i j))))))

(defun fill-pixel (data basins row col value)
  "Checks if ROWxCOL position in DATA is available for fill and fills that pixel
in BASIN. Returns (ROW . COL) if filled, NIL if not."
  (let ((rows (array-dimension data 0))
        (cols (array-dimension data 1)))
    (cond
      ((or (< row 0) (>= row rows)) nil)
      ((or (< col 0) (>= col cols)) nil)
      ;; pixel 9 in source array means a border
      ;; pixel VALUE in basins means that we already filled it
      (t (if (and (/= 9 (aref data row col))
                  (/= value (aref basins row col)))
             (progn
               (setf (aref basins row col) value)
               (cons row col))
             nil)))))

(defun fill-basin (data start-row start-col value
                   &key (basins (make-array
                                 (array-dimensions data)
                                 :element-type 'fixnum)))
  "Returns new array where all values are 0 except basin which starts at 
START-ROW, START-COL position. Basin is filled with VALUE."
  (let ((pixels (list (cons start-row start-col))))
    (loop while (not (null pixels))
          for (row . col) = (pop pixels)
          do (let ((top (fill-pixel data basins (1- row) col value))
                   (left (fill-pixel data basins row (1- col) value))
                   (bottom (fill-pixel data basins (1+ row) col value))
                   (right  (fill-pixel data basins row (1+ col) value)))
               (loop for p in (list top left bottom right)
                     when (not (null p)) do (push p pixels))))
    basins))

(defun part2 (path)
  ;; so first we find low points
  ;; then for each low point fill basin
  ;; then find biggest three and fill them
  (let* ((data (read-input path))
         (low-points nil)
         (basins (make-array (array-dimensions data) :element-type 'fixnum)))
    (loop for i from 0 below (array-dimension data 0) do
      (loop for j from 0 below (array-dimension data 1)
            when (is-low-point data i j)
              do (push (cons i j) low-points)))
    (loop for (row . col) in low-points
          for fill-value from 1
          do (fill-basin data row col fill-value :basins basins))
    ;; now we count basin size for each basin
    (let* ((basin-sizes (make-hash-table)))
      (loop for i from 0 below (array-dimension basins 0) do
        (loop for j from 0 below (array-dimension basins 1)
              for value = (aref basins i j)
              when (> value 0) do
                (incf (gethash value basin-sizes 0))))
      (let* ((sizes
               (sort
                (loop for key being the hash-keys of basin-sizes
                      collect (gethash key basin-sizes))
                #'>)))
        (format t "Result: ~a~%" 
                (* (car sizes) (cadr sizes) (caddr sizes)))))))

          
