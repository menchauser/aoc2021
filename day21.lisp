(defun die-func ()
  (let ((start 0))
    (lambda ()
      (setf start (1+ (mod start 100)))
      start)))

(defparameter +test-positions+ '(4 8))
(defparameter +input-positions+ '(6 3))

(defun play-deterministic (positions)
  ;; we roll endless loop until one of the scores exceeds 1000
  (labels ((move (position step)
             (1+ (mod (1- (+ position step)) 10))))
    (loop with (pos-1 pos-2) = positions
          with (score-1 score-2) = '(0 0)
          with die = (die-func)
          with total-rolls = 0
          for roll-1 = (+ (funcall die) (funcall die) (funcall die))
          for roll-2 = (+ (funcall die) (funcall die) (funcall die))
          do (incf total-rolls 3)
             (setf pos-1 (move pos-1 roll-1)
                   score-1 (+ score-1 pos-1))
             (format t "Player 1 rolls ~a and moves to space ~a, score: ~a~%"
                     roll-1 pos-1 score-1)
             (when (>= score-1 1000)
               (format t "Finished, 1 win!~%")
               (format t "Scores: ~a, ~a. Positions: ~a, ~a. Total rolls: ~a~%"
                       score-1 score-2 pos-1 pos-2 total-rolls)
               (return (* score-2 total-rolls)))
             (incf total-rolls 3)
             (setf pos-2 (move pos-2 roll-2)
                   score-2 (+ score-2 pos-2))
             (format t "Player 2 rolls ~a and moves to space ~a, score: ~a~%"
                     roll-2 pos-2 score-2)
             (when (>= score-2 1000)
               (format t "Finished, 2 win!~%")
               (format t "Scores: ~a, ~a. Positions: ~a, ~a. Total rolls: ~a~%"
                       score-1 score-2 pos-1 pos-2 total-rolls)
               (return (* score-1 total-rolls))))))

(defun part1 (path)
  (let* ((lines (uiop:read-file-lines path))
         (positions (mapcar
                     (lambda (line)
                       (parse-integer (subseq line (- (length line) 2))))
                     lines)))
    (play-deterministic positions)))
