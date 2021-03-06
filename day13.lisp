(defun read-input (path)
  "Returns dots as list of (X . Y) conses (first value) and fold instructions as
 second value."
  (with-open-file (input path)
    (let ((coords
            (loop for line = (read-line input nil)
                  while (and line (> (length line) 0))
                  for (x delim) = (multiple-value-list (read-from-string line))
                  for y = (read-from-string line t nil :start (1+ delim))
                  collect (cons x y)))
          (instructions
            (loop for line = (read-line input nil)
                  while line
                  collect (let* ((pos (1+ (position #\Space line :from-end t)))
                                 (coord (read-from-string line t nil
                                                          :start pos
                                                          :end (1+ pos)))
                                 (val (read-from-string line t nil
                                                        :start (+ pos 2))))
                            (cons coord val)))))
      (values coords instructions))))

(defun print-sheet (coords)
  (loop
    for c in coords
    maximize (car c) into max-x
    maximize (cdr c) into max-y
    finally 
       ;; now we prepare array and print it
       (let ((plane (make-array (list (1+ max-y) (1+ max-x))
                                :initial-element ".")))
         (loop for (x . y) in coords
               do (setf (aref plane y x) "#")
               finally
                  (loop for i from 0 below (array-dimension plane 0) do
                    (loop for j from 0 below (array-dimension plane 1) do
                      (format t "~a" (aref plane i j)))
                    (format t "~%"))))))
                  (format t "~a~%" plane)))))

(defun fold (input-dots inst)
  "Fold DOTS according to given INSTruction. Returns new dots."
  (let ((coord (car inst))
        (val (cdr inst))
        (dots (copy-list input-dots)))
    (format t "coord=~a, val=~a~%" coord val)
    (cond
      ((eql 'x coord)
       (format t "Fold vertically, x=~a~%" val)
       ;; now for every dot to the right of x we change its coord
       (loop for dot in dots
             for x = (car dot)
             when (> x val)
               do (setf (car dot)
                        (- val (- x val)))
             finally (return (remove-duplicates dots :test #'equal))))
      (t
       (format t "Fold horizontally, y=~a~%" val)
       ;; now for every dot below Y we change its coord
       (loop for dot in dots
             for y = (cdr dot)
             when (> y val)
               do (setf (cdr dot)
                        (- val (- y val)))
             finally (return (remove-duplicates dots :test #'equal)))))))

(defun part1 (path)
  (multiple-value-bind (dots instructions)
      (read-input path)
    (length (fold dots (car instructions)))))

(defun part2 (path)
  (multiple-value-bind (dots instructions)
      (read-input path)
    (loop for inst in instructions
          do (setf dots (fold dots inst))
          finally (print-sheet dots))))
