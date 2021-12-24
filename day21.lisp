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

(defun zeros (n)
  (loop for i from 1 to n collect 0))

(defstruct universe 
  positions   ;; list of player positions
  scores      ;; list of player scores
  ;; player numbers start from 0
  next-player
  won-player)

(defun new-universe (pos-1 pos-2) 
  (make-universe :positions (list pos-1 pos-2)
                 :scores (list 0 0)
                 :next-player 0))

(defun step-universe (u step &optional (won-threshold 21))
  (when (numberp (universe-won-player u))
    (error "Can not make step in won universe: ~a~%" u))
  (let* ((player (universe-next-player u))
         (pos (nth player (universe-positions u)))
         (score (nth player (universe-scores u)))
         (new-pos (move pos step))
         (new-score (+ score new-pos))
         (next-player (- 1 player))
         (new-positions (copy-list (universe-positions u)))
         (new-scores (copy-list (universe-scores u))))
    (setf (nth player new-positions) new-pos)
    (setf (nth player new-scores) new-score)
    (make-universe :positions new-positions
                   :scores new-scores
                   :next-player next-player
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
(defun roll (u count)
  (mapcar
   (lambda (outcome)
     (cons (step-universe u (car outcome)) (* count (cdr outcome))))
   +roll-outcomes+))

  
;; let's think about algo in general
;; we loop. for each step we first generate universes after 1st player rolled.
;; that will generate 27 new universes and only 7 different outcomes.
(defun play-dirac (positions)
  (let* ((init-universe (make-universe :positions positions
                                       :scores (zeros (length positions))
                                       :next-player 1))
         (unis (list (cons init-universe 1))))
    (loop with won-counts = (zeros (length positions))
          while unis
          ;; on each step we produce new universes
          ;; from won universe only itself is produced
          ;; from not-yet-won universe - new ones generated
          do (format t "Current unis: ~a~%" (length unis))
             (loop for (uni . count) in unis
                   for won-player = (universe-won-player uni)
                   if (null won-player)
                     append (roll uni count) into new-unis
                   else
                     do (incf (nth won-player won-counts) count)
                   finally
                      (setf unis new-unis)
                      (format t "New unis: ~a~%" (length unis))
                      (format t "New won counts: ~a~%" won-counts))
          finally
             (format t "final unis count: ~a~%" (length unis))
             (format t "Total won: ~a~%" won-counts)
             (format t "total number: ~a~%"
                     (loop for (u . c) in unis
                           sum c))
             (return unis)
          )))

(defun part1 (path)
  (let* ((lines (uiop:read-file-lines path))
         (positions (mapcar
                     (lambda (line)
                       (parse-integer (subseq line (- (length line) 2))))
                     lines)))
    (play-deterministic positions)))
