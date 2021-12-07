(defun read-input (path)
  (read-from-string
   (concatenate 'string "("
                (substitute #\Space #\, (uiop:read-file-line path))
                ")")))

(defun total-fuel (pos crabs)
  "Calculate total fuel needed for CRABS to move in position POS."
  (reduce #'+
          (mapcar (lambda (c) (abs (- c pos))) crabs)))

(defun total-fuel-2 (pos crabs)
  "Calculate total fuel needed for CRABS to move in position POS using part-2 
movement model."
  (labels ((fuel (crab)
             (let ((steps (abs (- crab pos))))
               (/ (* (+ steps 1) steps) 2))))
    (reduce #'+ (mapcar #'fuel crabs))))

(defun solution (path fuel-function)
  (let* ((crabs (read-input path))
         (max-pos (apply #'max crabs)))
    (loop for pos from 0 to max-pos
          minimize (funcall fuel-function pos crabs))))

(defun part1 (path)
  (solution path #'total-fuel))

(defun part2 (path)
  (solution path #'total-fuel-2))
