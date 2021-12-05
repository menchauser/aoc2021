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

(defun type-of-line (line)
  "Check if input line in format (X1 Y1 X2 Y2) is HORIZONTAL or VERTICAL. Return
NIL if neither."
  (destructuring-bind (x1 y1 x2 y2) line
    (cond
      ((equal x1 x2) 'vertical)
      ((equal y1 y2) 'horizontal)
      (t nil))))

(defun max-x (input)
  (max
   (reduce #'max (mapcar #'car input))
   (reduce #'max (mapcar #'caddr input))))

(defun max-y (input)
  (max
   (reduce #'max (mapcar #'cadr input))
   (reduce #'max (mapcar #'cadddr input))))

(defun make-diagram (width height)
  "Create list of lists of zeros with WIDTH number of cols and HEIGHT number of
rows (lists)."
  (loop for j from 1 to height
        collect (loop for i from 1 to width collect 0)))

;; we read input
;; then find max X and max Y and construct list of lists of 0 of that size
;; than go over all hor and vert lines and INCF fields
(defun part1 (path)
  (let* ((input (read-input path))
         (diagram (make-diagram (1+ (max-x input))
                                (1+ (max-y input)))))
    (format t "Using diagram with size ~d x ~d~%"
            (length (car diagram)) (length diagram))
    (dolist (line input)
      (destructuring-bind (x1 y1 x2 y2) line
        (cond
          ;; vertical-line
          ((equal x1 x2) 
           (format t "Vertical: ~d,~d -> ~d,~d~%" x1 y1 x2 y2)
           (when (< y2 y1) (rotatef y1 y2))
           (loop for y from y1 to y2
                 do (incf (elt (elt diagram y) x1))))
          ;; horizontal-line
          ((equal y1 y2)
           (format t "Horizontal: ~d,~d -> ~d,~d~%" x1 y1 x2 y2)
           (when (< x2 x1) (rotatef x1 x2))
           (loop for x from x1 to x2
                 do (incf (elt (elt diagram y1) x))))
          ;; otherwise we do nothing
          (t (format t "Unsupported: ~d,~d -> ~d,~d~%" x1 x2 y1 y2)))))
    ;; now we count number of points > 1
    (reduce #'+
            (mapcar (lambda (row) (count-if (lambda (x) (> x 1)) row))
                    diagram))))
