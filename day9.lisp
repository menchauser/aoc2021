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
