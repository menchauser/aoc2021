(defun read-input (path)
  (let ((lines (uiop:read-file-lines path)))
    (mapcar
     (lambda (line)
       (let ((delim-pos (position #\- line)))
         (cons (subseq line 0 delim-pos) (subseq line (1+ delim-pos)))))
     lines)))

(defun to-map (input)
  (append input
          (mapcar (lambda (edge) (cons (cdr edge) (car edge))) input)))

(defun connections (map cave)
  "Return list of all caves connected to CAVE according to MAP."
  (mapcar #'cdr
          (remove-if-not (lambda (c) (equal cave (car c))) map)))

(defun print-path (path)
  (format t "~{~a~^,~}" (reverse path)))

(defun all-small-caves (map)
  (set-difference 
   (remove-duplicates (remove-if-not #'smallp (mapcar #'car map)) :test #'equal)
   '("start" "end")
   :test #'equal))

(defun smallp (cave)
  (and (lower-case-p (char cave 0))
       cave))

(defun visited (cave visited-path)
  (and (smallp cave)
       (member cave visited-path :test #'equal)))

(defun select-caves (candidates current-path extra-cave)
  ;; visited caves may contain only one element
  ;; if we already have visited cave: check that current != visited
  ;; otherwise: check by usual means
  (remove-if
   (lambda (c) 
     (visited c (remove extra-cave current-path :test #'equal :count 1)))
   candidates))

(defun collect-paths (map
                      current-path
                      finished-paths
                      &optional (extra-cave nil))
  "This function counts how many paths are available through the MAP when
CURRENT-PATH already walked. CURRENT-PATH is a list which head is last visited
cave. MAP is an alist which describes cave system. EXTRA-CAVE determines which
cave could be visited twice."
  ;;(format t "run~%")
  (if (equal "end" (car current-path))
      (if (member current-path finished-paths :test #'equal)
          (progn
            ;; (format t " | already traced~%")
            nil)
          (progn
            ;;(print-path current-path)
            ;;(format t " | finished~%")
            (list current-path)))
      (let* ((candidates (connections map (car current-path)))
             (next-caves (select-caves candidates current-path extra-cave)))
        ;;(format t "candidates: ~a~%selected: ~a~%" candidates next-caves)
        (if (null next-caves)
            (progn
              ;; (print-path current-path)
              ;; (format t " | dead end~%")
              nil)
            (progn
             ;; (print-path current-path)
            (loop for cave in next-caves
                  append (collect-paths map
                                        (cons cave current-path)
                                        finished-paths
                                        extra-cave)))))))

(defun part1 (input)
  (let ((map (to-map (read-input input))))
    (length (collect-paths map '("start") nil))))

(defun part2 (input)
  (let* ((map (to-map (read-input input)))
         (small-caves (all-small-caves map))
         (finished-paths nil))
    (format t "extra caves: ~a~%" small-caves)
    (loop for extra-cave in (cons nil small-caves)
          for next-paths = (collect-paths map '("start")
                                          finished-paths
                                          extra-cave)
          sum (length next-paths) into total-count
          do (progn
               (format t "finished for extra cave: ~a~%" extra-cave)
               (setf finished-paths (append finished-paths next-paths)))
          finally (return total-count))))
