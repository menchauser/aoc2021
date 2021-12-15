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
          with row = max-row
          with col = max-col
          with risk = (aref cave row col)
          do (progn
               (cond
                 ((and (= row 0) (= col 0)) (return risk))
                 ;; if we hit top or left line: just move along it
                 ((= row 0)
                  (decf col)
                  (format t "(hor) next step: ~a, ~a; delta: ~a~%"
                          row col (aref cave row col))
                  (incf risk (aref cave row col)))
                 ((= col 0)
                  (decf row)
                  (format t "(ver) next step: ~a, ~a; delta: ~a~%"
                          row col (aref cave row col))
                  (incf risk (aref cave row col)))
                 ;; otherwise: select cell with minimal risk
                 (t (if (< (aref cave row (1- col)) (aref cave (1- row) col))
                        (decf col)
                        (decf row))
                    (format t "next step: ~a, ~a; delta: ~a~%"
                            row col (aref cave row col))
                    (incf risk (aref cave row col)))))
          finally (return risk))))
