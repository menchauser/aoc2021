(defparameter +debug+ t)

(defmacro info (control-string &rest args)
  `(when +debug+
     (format t ,control-string ,@args)))

(defun read-input (path)
  (let* ((line (uiop:read-file-line path))
         (x1-start (1+ (position #\= line)))
         (x1-end (position #\. line))
         (x2-end (position #\, line))
         (y1-start (1+ (position #\= line :from-end t)))
         (y1-end (1- (position #\. line :from-end t))))
    (list
     (parse-integer (subseq line x1-start x1-end))
     (parse-integer (subseq line (+ 2 x1-end) x2-end))
     (parse-integer (subseq line y1-start y1-end))
     (parse-integer (subseq line (+ 2 y1-end))))))

(defun hitp (x y target)
  (destructuring-bind (x1 x2 y1 y2)
      target
    (and (<= x1 x x2)
         (<= y1 y y2))))

(defun launch-probe (vx vy target)
  "VX and VY are integers with starting velocity. TARGET is a list describing 
target area: (X1 X2 Y1 Y2)."
  ;; for now let's just print steps and check when we finished (inside the
  ;; target area or undershoot/overshoot
  (loop with x = 0 and y = 0 and max-y = 0
        for (x1 x2 y1 y2) = target
        do ;; (info "at: ~a, ~a~%" x y)
           (setf max-y (max y max-y))
        when (hitp x y target)
          return (list 'hit x y max-y)
        ;; update positions and speeds
        do (setf x (+ x vx)
                 y (+ y vy)
                 vx (max (1- vx) 0)
                 vy (1- vy))
        ;; check undershoot/overshoot
        when (and (< y y1) (<= x x1))
          return (list 'undershoot x y)
        ;; maybe put it in separate status?
        when (< y y1)
          return (list 'undershoot x y)
        when (> x x2)
          return (list 'overshoot x y)))

(defun find-highest-y (target)
  (loop with max-y = 0
        with x1 = (car target) and x2 = (cadr target)
        ;; actually starting x should be such as we don't undershoot 
        ;; for that we need to find min length of arithmetic progression of VXs.
        ;; one way to find it is use positive root of quadratic inequality:
        ;;  vx^2 + vx - 2 * x1 >= 0
        with start-vx = (ceiling (/ (- (sqrt (+ 1 (* 8 x1))) 1) 2))
        for vx from start-vx to x2
        do (loop for vy from 1 to 1000 ;; empirically choose possible max y
                 for (status x y top-y) = (progn
                                            (info "probe: vx=~a, vy=~a -> " vx vy)
                                            (launch-probe vx vy target))
                 do (ecase status
                      (hit
                       (info "hit!~%")
                       (setf max-y (max max-y top-y)))
                      (overshoot
                       (info "overshoot~%")
                       (return))
                      (undershoot
                       (info "undershoot~%"))))
        finally (return max-y)))
  
(defun part1 (path)
  (let ((+debug+ nil))
    (find-highest-y (read-input path))))
