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
  (lower-case-p (char cave 0)))

(defun visited (cave visited-path)
  (and (smallp cave)
       (member cave visited-path :test #'equal)))

(defun select-caves (candidates current-path extra-cave)
  ;; if we have visited extra-cave: don't check it first time
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
  (if (equal "end" (car current-path))
      (list current-path)
      ;; (if (member current-path finished-paths :test #'equal)
          ;;nil
      ;;(list current-path))
      (let* ((candidates (connections map (car current-path)))
             (next-caves (select-caves candidates current-path extra-cave)))
        (if (null next-caves)
            nil
            (loop for cave in next-caves
                  append (collect-paths map
                                        (cons cave current-path)
                                        finished-paths
                                        extra-cave))))))

(defun part1 (input)
  (let ((map (to-map (read-input input))))
    (length (collect-paths map '("start") nil))))

(defun part2 (input)
  (let* ((map (to-map (read-input input)))
         (small-caves (all-small-caves map))
         (finished-paths nil))
    (format t "extra caves: ~a~%" small-caves)
    (loop for extra-cave in (cons nil small-caves)
          append (collect-paths map '("start")
                                finished-paths
                                extra-cave)
          into result 
          do (format t "finished for extra cave: ~a~%" extra-cave)
          finally (return
                    (let ((hashresult (make-hash-table :test #'equal)))
                      (loop for x in result
                            do (setf (gethash x hashresult) t)
                            finally (return (hash-table-size hashresult))))))))
                          
