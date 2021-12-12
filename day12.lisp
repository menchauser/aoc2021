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

(defun visited (cave visited-path)
  (and (lower-case-p (char cave 0))
       (member cave visited-path :test #'equal)))

(defun print-path (path)
  (format t "~{~a~^,~}" (reverse path)))

(defun count-paths (map current-path)
  "This function counts how many paths are available through the MAP when
CURRENT-PATH already walked. CURRENT-PATH is a list which head is last visited
cave. MAP is an alist which describes cave system."
  (if (equal "end" (car current-path))
      (progn
        (print-path current-path)
        (format t " | finished~%")
        1)
      (let* ((next-cave-candidates (connections map (car current-path)))
             (next-caves (remove-if (lambda (c) (visited c current-path))
                                    next-cave-candidates)))
        (if (null next-caves)
            (progn
              (print-path current-path)
              (format t " | dead end~%")
              0)
            (loop for cave in next-caves
                  sum (count-paths map (cons cave current-path)))))))
    
(defun part1 (input)
  (let ((map (to-map (read-input input))))
    (count-paths map '("start"))))
