(defun read-input (path)
  (let ((lines (uiop:read-file-lines path)))
    (make-array (list (length lines) (length (car lines)))
                :initial-contents
                (mapcar (lambda (line)
                          (mapcar (lambda (c) (- (char-int c) (char-int #\0)))
                                  (coerce line 'list)))
                        lines))))

(defun day15 (path)
  (let ((cave (read-input path)))
    ;; we start at bottom-right position and go top-left until 1-1
    (loop with max-row = (1- (array-dimension test15 0))
          with max-col = (1- (array-dimension test15 1))
          with row = 0
          with col = 0
          with risk = (aref cave row col)
          do (progn
               (cond
                 ((and (= row max-row) (= col max-col)) (return risk))
                 ;; if we hit top or left line: just move along it
                 ((= row max-row)
                  (incf col)
                  (format t "(hor) next step: ~a, ~a; delta: ~a~%"
                          row col (aref cave row col))
                  (incf risk (aref cave row col)))
                 ((= col max-col)
                  (incf row)
                  (format t "(ver) next step: ~a, ~a; delta: ~a~%"
                          row col (aref cave row col))
                  (incf risk (aref cave row col)))
                 ;; otherwise: select cell with minimal risk
                 (t (if (< (aref cave row (1+ col)) (aref cave (1+ row) col))
                        (incf col)
                        (incf row))
                    (format t "next step: ~a, ~a; delta: ~a~%"
                            row col (aref cave row col))
                    (incf risk (aref cave row col)))))
          finally (return risk))))
