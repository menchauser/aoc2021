(defparameter +brackets+
  '((#\( . #\))
    (#\[ . #\])
    (#\{ . #\})
    (#\< . #\>)))

(defparameter +scores+
  '((#\) . 3)
    (#\] . 57)
    (#\} . 1197)
    (#\> . 25137)))

(defparameter +complete-scores+
  '((#\( . 1)
    (#\[ . 2)
    (#\{ . 3)
    (#\< . 4)))

(defun solution (path)
  (let ((lines (uiop:read-file-lines path))
        (complete-scores nil))
    (loop
      for line in lines
      for stack = nil
      for complete-score = 0
      for corrupt-score = (loop
                            for ch in (coerce line 'list)
                            do (if (assoc ch +brackets+)
                                   (push ch stack)
                                   (let ((top-ch (pop stack)))
                                     (when (not (eql (car (rassoc ch +brackets+)) top-ch))
                                       (return (cdr (assoc ch +scores+))))))
                            finally (return 0))
      ;; not corrupt, just incomplete
      when (= 0 corrupt-score)
        do (loop
             for ch in stack
             do (setf complete-score (+ (* complete-score 5)
                                        (cdr (assoc ch +complete-scores+))))
             finally (push complete-score complete-scores))
      sum corrupt-score into total-corrupt-score
      finally (progn
                (format t "Corrupt score: ~a~%" total-corrupt-score)
                (setf complete-scores (sort complete-scores #'<))
                (format t "Resulting auto-complete score: ~a~%"
                        (elt complete-scores
                             (floor (/ (length complete-scores) 2))))))))
