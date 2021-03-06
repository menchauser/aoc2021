(defun parse-instruction (line)
  (multiple-value-bind (command pos)
      (read-from-string line)
    (let ((value (read-from-string line t nil :start pos)))
      (cons command value))))
    
(defun read-course-from-file (path)
  "Parses a file for day2 and returns list of pairs: instruction . value"
  (let ((data (uiop:read-file-lines path)))
    (mapcar #'parse-instruction data)))

(defun part1 (path)
  (let ((course (read-course-from-file path))
        (horizontal 0)
        (depth 0))
    (dolist (instr course)
      (let ((command (car instr))
            (value (cdr instr)))
        (cond ((eql command 'forward) (incf horizontal value))
              ((eql command 'up) (decf depth value))
              ((eql command 'down) (incf depth value)))))
    (* horizontal depth)))

(defun part2 (path)
  (let ((course (read-course-from-file path))
        (horizontal 0)
        (depth 0)
        (aim 0))
    (dolist (instr course)
      (let ((command (car instr))
            (value (cdr instr)))
        (cond ((eql command 'forward)
               (incf horizontal value)
               (incf depth (* value aim)))
              ((eql command 'up) (decf aim value))
              ((eql command 'down) (incf aim value)))))
    (* horizontal depth)))
