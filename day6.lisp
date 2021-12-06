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

(defun read-numbers (s)
  (read-from-string (concatenate 'string "(" (substitute #\Space #\, s) ")")))

(defun part1 (days path)
  (let* ((input (uiop:read-file-line path))
         (fish-list (read-numbers input)))
    (fish-loop days fish-list)))
    
