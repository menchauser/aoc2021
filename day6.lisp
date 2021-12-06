(defun run-day (fish-list)
  "Process one day. If timer > 0 decrease it, if timer = 0 - make it 6 and add
new 8."
  (let* ((new-fish nil)
         (existing-fish
           (mapcar
            (lambda (fish)
              (if (plusp fish)
                  (1- fish)
                  (progn
                    (push 8 new-fish)
                    6)))
            fish-list)))
    (append existing-fish new-fish)))
  
(defun fish-loop (days fish-list)
  (let ((current-fish (copy-list fish-list)))
    (dotimes (i days)
      (setf current-fish (run-day current-fish)))
    (length current-fish)))

(defun fish-loop-opt (days fish-list)
  ;; so we need to calculate first 9 sequence numbers manually
  (let ((seq (reverse (loop for days from 0 to 9 collect (fish-loop days fish-list)))))
    ;; and then we can calculate by fibonacci with steps -7, -9
    ;; starting from list-end: next element -
    (format t "initial sequence: ~a~%" seq)
    (dotimes (i (- days 9))
      (let* ((next (+ (elt seq 6) (elt seq 8))))
        (setf seq (cons next seq))))
    (car seq)))

(defun read-numbers (s)
  (read-from-string (concatenate 'string "(" (substitute #\Space #\, s) ")")))

(defun solution (days path)
  (let* ((input (uiop:read-file-line path))
         (fish-list (read-numbers input)))
    (fish-loop-opt days fish-list)))
    
