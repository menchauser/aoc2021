(defun die-func ()
  (let ((start 0))
    (lambda ()
      (setf start (1+ (mod start 100)))
      start)))

(defparameter +test-positions+ '(4 8))
(defparameter +input-positions+ '(6 3))

(defun move (position step)
  (1+ (mod (1- (+ position step)) 10)))

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

;; Part 2

(defun zeros (n)
  (loop for i from 1 to n collect 0))

(defstruct universe 
  positions   ;; list of player positions
  scores      ;; list of player scores
  ;; player numbers start from 0
  won-player)

(defun new-universe (pos-1 pos-2) 
  (make-universe :positions (list pos-1 pos-2)
                 :scores (list 0 0)))

(defun step-universe (player u step &optional (won-threshold 21))
  (when (numberp (universe-won-player u))
    (error "Can not make step in won universe: ~a~%" u))
  (let* ((pos (nth player (universe-positions u)))
         (score (nth player (universe-scores u)))
         (new-pos (move pos step))
         (new-score (+ score new-pos))
         (new-positions (copy-list (universe-positions u)))
         (new-scores (copy-list (universe-scores u))))
    (setf (nth player new-positions) new-pos)
    (setf (nth player new-scores) new-score)
    (make-universe :positions new-positions
                   :scores new-scores
                   :won-player
                   (position-if (lambda (x) (>= x won-threshold)) new-scores))))

;; 3-roll result and number of such results
(defparameter +roll-outcomes+
  '((3 . 1)
    (4 . 3)
    (5 . 6)
    (6 . 7)
    (7 . 6)
    (8 . 3)
    (9 . 1)))

;; every time we throw: we generate 27 universes (with 7 really different
;; outcomes)
(defun roll (player u count)
  (mapcar
   (lambda (outcome)
     (cons (step-universe player u (car outcome)) (* count (cdr outcome))))
   +roll-outcomes+))

(defparameter +hash-size+ 10000)

(defun play-dirac (positions)
  (let* ((init-uni (make-universe :positions positions
                                  :scores (zeros (length positions))))
         (next-player 0)
         (unis (make-hash-table :test #'equalp :size +hash-size+))
         (new-unis (make-hash-table :test #'equalp :size +hash-size+)))
    (setf (gethash init-uni unis) 1)
    (loop with won-counts = (zeros (length positions))
          while (> (hash-table-count unis) 0)
          ;; on each step we produce new universes
          ;; from won universe only itself is produced
          ;; from not-yet-won universe - new ones generated
          do (format t "Current unis: ~a~%" (hash-table-count unis))
             (loop for uni being the hash-keys in unis using (hash-value count)
                   ;; for every universe we create new universes
                   do (loop 
                        for (new-uni . new-count) in (roll next-player uni count)
                        for won-player = (universe-won-player new-uni)
                        do (if (null won-player)
                               (incf (gethash new-uni new-unis 0) new-count)
                               (incf (nth won-player won-counts) new-count)))
                   finally
                      (setf next-player (- 1 next-player))
                      (setf unis new-unis)
                      (setf new-unis (make-hash-table :test #'equalp :size +hash-size+))
                      (format t "New unis: ~a~%" (hash-table-count unis))
                      (format t "New won counts: ~a~%" won-counts))
          finally
             (format t "final unis count: ~a~%" (hash-table-count unis))
             (format t "Total won: ~a~%" won-counts)
             (format t "Max: ~a~%" (apply #'max won-counts)))))

(defun part1 (path)
  (let* ((lines (uiop:read-file-lines path))
         (positions (mapcar
                     (lambda (line)
                       (parse-integer (subseq line (- (length line) 2))))
                     lines)))
    (play-deterministic positions)))

(defun part2 (path)
  (let* ((lines (uiop:read-file-lines path))
         (positions (mapcar
                     (lambda (line)
                       (parse-integer (subseq line (- (length line) 2))))
                     lines)))
    (play-dirac positions)))
