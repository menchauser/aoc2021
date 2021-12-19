(defun flatten (xs)
  (cond ((null xs) nil)
        ((atom xs) (list xs))
        (t (loop for x in xs appending (flatten x)))))

(defun read-input (s)
  "Reads snail-number string S into cons-cell."
  (loop with result = (coerce s 'list)
        for (from to) in '((#\[ #\()
                           (#\] #\))
                           (#\, (#\Space #\. #\Space)))
        do (setf result (nsubst to from result))
        finally (return (read-from-string
                         (coerce (flatten result) 'string)))))

(defun to-flat-number (ns &optional (level 0))
  "Converts cons-number representation to flat-number. Flat-number is an alist 
of of (number . level). Example:
[[[1,2],3],4] -> ((1 . 3), (2 . 3), (3 . 2), (4 . 1)."
  (declare (fixnum level))
  (cond ((null ns) nil)
        ((atom ns) (list (cons ns level)))
        (t (append (to-flat-number (car ns) (1+ level))
                   (to-flat-number (cdr ns) (1+ level))))))

(defun read-flat (s)
  (to-flat-number (read-input s)))

(defun explode-pos (ns)
  (position-if (lambda (n) (> (cdr n) 4)) ns))

(defun split-pos (ns)
  (position-if (lambda (n) (>= (car n) 10)) ns))

(defun run-explode (ns)
  ;; we find the first level > 4
  (let* ((rs (copy-alist ns))
         (lpos (explode-pos rs)))
    (when lpos
      ;; find the pair and explode it
      (let ((new-level (1- (cdr (nth lpos rs))))
            (rpos (1+ lpos)))
        (when (> lpos 0)
          (incf (car (nth (1- lpos) rs))
                (car (nth lpos rs))))
        (when (< rpos (1- (list-length rs)))
          (incf (car (nth (1+ rpos) rs))
                (car (nth rpos rs))))
        (setf rs (remove (nth rpos rs) rs :test #'equal :count 1))
        (setf (car (nth lpos rs)) 0
              (cdr (nth lpos rs)) new-level)
        rs))))

(defun run-split (ns)
  ;; we find first number >= 10
  (let* ((rs (copy-alist ns))
         (pos (split-pos rs)))
    (when pos
      ;; we replace the cell and push new one after it
      (let* ((old-cell (nth pos rs))
             (new-val (/ (car old-cell) 2))
             (new-level (1+ (cdr old-cell))))
        (setf (nth pos rs) (cons (floor new-val) new-level))
        (push (cons (ceiling new-val) new-level)
              (cdr (nthcdr pos rs))))
      rs)))

(defun nrun-reduce-number (rs)
  ;; while we have level > 4 or number >= 10: we continue working
  (let (ran)
    ;; first try to explode until we can
    (loop for next-rs = (run-explode rs)
          while next-rs
          do (format t "ran explode: ~a~%" next-rs)
             (setf rs next-rs
                   ran t))
    ;; if no explode possible: try running split and repeat reduce
    (let ((next-rs (run-split rs)))
      (when next-rs
        (format t "ran split: ~a~%" next-rs)
        (setf rs next-rs
              ran t)))
    (if ran
        (run-reduce-number rs)
        rs)))
        
(defun run-reduce-number (ns)
  (let ((rs (copy-alist ns)))
    (nrun-reduce-number rs)))

(defun add (ans bns)
  "Adds and reduces two snail numbers in flat presentation."
  ;; to add two number we concat them and increment their levels
  (let ((result (append ans bns)))
    (dolist (n result)
      (incf (cdr n)))
    (format t "raw add result: ~a~%" result)
    (run-reduce-number result)))

(defun pair-mag (a b)
  (cons
   (+ (* 3 (car a)) (* 2 (car b)))
   (1- (cdr a))))

(defun magnitude (ns)
  ;; (let ((rs (copy-alist ns)))
  (format t "next ns: ~a~%" ns)
  ;; (read-line)
  (cond
    ((null ns) 0)
    ((= 1 (length ns)) ns)
    (t
     (let ((left (car ns))
           (right (cadr ns)))
       ;; pair found
       (if (= (cdr left) (cdr right))
           (progn
             (format t "found pair: ~a, ~a~%" left right)
             (setf (car ns) (pair-mag left right)
                   ns (remove right ns :test #'equal :count 1)))
           ;; else: we skip to remaining parts
           (progn
             (format t "unequal levels: ~a, ~a~%" left right)
             (setf (cdr ns) (magnitude (cdr ns)))))
       (magnitude ns)))))

(defun unflatten-number (ns)
  ;; to unflatten on each step we find 
  (loop with result = nil and curr-level = 0 and left = nil
        for n in ns
        for delta = (- (cdr n) curr-level)
        do (format t "process: ~a~%" n)
        when (> delta 0)
          do (loop repeat delta do (push #\( result))
             (push #\# result)
             (setf curr-level (cdr n))
        when (< delta 0)
          do (loop repeat (abs delta) do (push #\) result))
             (setf curr-level (cdr n))
        when (= delta 0)
          do (push #\. result)
             (push #\# result)
        finally (return (coerce (reverse result) 'string))))
           
(defun part1 (path)
  (let* ((lines (uiop:read-file-lines path))
         (nums (mapcar #'read-flat lines)))
    (reduce #'add nums)))
