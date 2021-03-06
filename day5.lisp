(defun read-list (s)
  "Convert comma-separated list of integers to s-exp list."
  (read-from-string
   (concatenate 'string
                "(" (substitute #\Space #\, s) ")")))

(defun parse-line (str)
  (let* ((first-pair-end (position #\Space str))
         (second-pair-start (+ first-pair-end 4))
         (first-pair (subseq str 0 first-pair-end))
         (second-pair (subseq str second-pair-start)))
    (append (read-list first-pair) (read-list second-pair))))

(defun read-input (path)
  (mapcar #'parse-line (uiop:read-file-lines path)))

(defun max-x (input)
  (max
   (reduce #'max (mapcar #'car input))
   (reduce #'max (mapcar #'caddr input))))

(defun max-y (input)
  (max
   (reduce #'max (mapcar #'cadr input))
   (reduce #'max (mapcar #'cadddr input))))

;; we read input
;; then find max X and max Y and construct list of lists of 0 of that size
;; than go over all hor and vert lines and INCF fields
(defun part1 (path)
  (let* ((input (read-input path))
         (diagram (make-array (list (1+ (max-y input))
                                    (1+ (max-x input))))))
    (dolist (line input)
      (destructuring-bind (x1 y1 x2 y2) line
        (cond
          ;; vertical-line
          ((equal x1 x2) 
           (when (< y2 y1) (rotatef y1 y2))
           (loop for y from y1 to y2
                 do (incf (aref diagram y x1))))
          ;; horizontal-line
          ((equal y1 y2)
           (when (< x2 x1) (rotatef x1 x2))
           (loop for x from x1 to x2
                 do (incf (aref diagram y1 x))))
          ;; otherwise we do nothing
          (t nil))))
    ;; now we count number of points > 1
    (loop for i from 0 below (array-dimension diagram 0) sum
      (loop for j from 0 below (array-dimension diagram 1)
            count (> (aref diagram i j) 1)))))

(defun part2 (path)
  (let* ((input (read-input path))
         (diagram (make-array (list (1+ (max-y input))
                                    (1+ (max-x input))))))
    (dolist (line input)
      (destructuring-bind (x1 y1 x2 y2) line
        (cond
          ;; vertical-line
          ((equal x1 x2) 
           (when (< y2 y1) (rotatef y1 y2))
           (loop for y from y1 to y2
                 do (incf (aref diagram y x1))))
          ;; horizontal-line
          ((equal y1 y2)
           (when (< x2 x1) (rotatef x1 x2))
           (loop for x from x1 to x2
                 do (incf (aref diagram y1 x))))
          ;; otherwise lines are diagonal
          (t
           ;; let's at least order by coordinate X and then choose direction by Y
           (when (< x2 x1)
             (rotatef x1 x2)
             (rotatef y1 y2))
           (if (< y1 y2)
               ;; first scenario: top-left -> bottom-right
               (loop for y from y1 to y2
                     for x from x1 to x2
                     do (incf (aref diagram y x)))
               ;; otherwise: bottom-left -> top-right
               (loop for y from y1 downto y2
                     for x from x1 to x2
                     do (incf (aref diagram y x))))))))
    (loop for i from 0 below (array-dimension diagram 0) sum
      (loop for j from 0 below (array-dimension diagram 1)
            count (> (aref diagram i j) 1)))))
