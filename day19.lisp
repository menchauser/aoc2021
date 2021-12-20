(defparameter +debug+ t)

(defmacro info (control-string &rest args)
  (when +debug+
     `(format t ,control-string ,@args)))

(defun read-input (path)
  ;; we return list of arrays of scanner reports
  (labels ((read-scanner (in)
             ;; read list of lists of coordinates
             (loop with result = nil
                   for line = (read-line in nil)
                   while (> (length line) 0)
                   when (not (char= (char line 4) #\s))
                     do (let ((sep1 (position #\, line :test #'char=))
                              (sep2 (position #\, line :test #'char= :from-end t)))
                          (push
                           (list
                            (parse-integer line :start 0 :end sep1)
                            (parse-integer line :start (1+ sep1) :end sep2)
                            (parse-integer line :start (1+ sep2)))
                           result))
                   finally (return (nreverse result)))))
    (with-open-file (in path)
      (loop with full-report = nil
            for next-scanner = (read-scanner in)
            while next-scanner
            do (push next-scanner full-report)
            finally (return (nreverse full-report))))))


(defun set-intersection (list-1 list-2)
  (loop for x in list-1
        when (member x list-2 :test #'equal)
          collect x))

;; counter-clockwise: pi/2, pi, 3pi/2
(defparameter rot-cosinuses '(0 -1 0))
(defparameter rot-sinuses   '(1 0 -1))

(defparameter +rot-left-1+ '((0 -1)
                             (1 0)))

(defparameter +rot-left-2+ '((-1 0)
                             (0 -1)))

(defparameter +rot-left-3+ '((0 1)
                             (-1 0)))

(defparameter +rot-matrices-2d+
  (list +rot-left-1+ +rot-left-2+ +rot-left-3+))

(defparameter +rot-cossins+
  ;;   0      π/2       π     3*π/2
  '((1 . 0) (0 . 1) (-1 . 0) (0 . -1)))


(defun matmul2d (mat vec)
  "Perform matrix multiplication of matrix to vector. Matrix is a list of lists,
vector is a list."
  (loop for row in mat
        collect (loop for a in row
                      for x in vec
                      sum (* a x))))

(defun rotations2d (vec)
  (loop with (x y) = vec
        for (cosa . sina) in +rot-cossins+
        do (info "x=~a, y=~a, cos=~a, sin=~a~%"
                 x y cosa sina)
        collect (list (- (* x cosa) (* y sina))
                      (+ (* x sina) (* y cosa)))))

(defun rep-rotations2d (rep)
  "Generate all possible rotations for list of coordinates."
  (loop for (c . s) in +rot-cossins+
        collect (loop for (x y) in rep
                      collect (list (- (* x c) (* y s))
                                    (+ (* x s) (* y c))))))

(defun rep-rotations3d (report )
  ;; we have 3 angles and for each one we can generate matmul
  (loop for (ca . sa) in +rot-cossins+
        append
        (loop for (cb . sb) in +rot-cossins+
              append
              (loop for (cc . sc) in +rot-cossins+
                    for r11 = (* ca cb)
                    for r12 = (- (* ca sb sc) (* sa cc))
                    for r13 = (+ (* ca sb cc) (* sa sc))
                    for r21 = (* sa cb)
                    for r22 = (+ (* sa sb sc) (* ca cc))
                    for r23 = (- (* sa sb cc) (* ca sc))
                    for r31 = (- sb)
                    for r32 = (* cb sc)
                    for r33 = (* cb cc)
                    collect
                    (loop for (x y z) in report
                          collect
                          (list (+ (* x r11) (* y r12) (* z r13))
                                (+ (* x r21) (* y r22) (* z r23))
                                (+ (* x r31) (* y r32) (* z r33))))))
          into rotations
        finally (return (remove-duplicates rotations :test #'equal))))

;; we have two lists of coordinates: we should find out if they have overlapping
(defun overlaps2d (rep-1 rep-2 total)
  "Checks if we can find TOTAL overlapping points between reports REP-1 and 
REP-2. Coordinate system is shifted between two reports."
  ;; let us find out borders of both reports and then try to move right report
  ;; and check how much overlaps
  ;; we start by setting top-right corner of rep2 to bottom-left corner of rep1
  ;; and then we move rep2 right and up, row by row
  ;; find coordinates of rep1's bottom-left corner 
  (labels ((borders (report)
             (loop for (x y) in report
                   minimize x into min-x 
                   minimize y into min-y
                   maximize x into max-x
                   maximize y into max-y
                   finally (return (list min-x min-y max-x max-y))))
           (offset (report dx dy)
             (loop for (x y) in report
                   collect (list (+ x dx) (+ y dy)))))
    ;; we find borders of both report coordinates, calculate min and max shift
    ;; for rep-2 coordinates across rep-1
    (let* ((borders-1 (borders rep-1))
           (borders-2 (borders rep-2))
           (dx-start (- (car borders-1) (caddr borders-2)))
           (dx-end (- (caddr borders-1) (car borders-2)))
           (dy-start (- (cadr borders-1) (cadddr borders-2)))
           (dy-end (- (cadddr borders-1) (cadr borders-2))))
      (info "borders 1: ~a~%" borders-1)
      (info "borders 2: ~a~%" borders-2)
      (info "dx: ~a..~a~%" dx-start dx-end)
      (info "dy: ~a..~a~%" dy-start dy-end)
      ;; to start we shift rep-2's top-right corner to rep-1's bottom-left
      ;; corner and finish by putting rep-2's bottom-left corner to rep-1's
      ;; top-right corner. we move rows up and right.
      (loop named outer for dx from dx-start to dx-end
            do (loop for dy from dy-start to dy-end
                         for shifted-rep-2 = (offset rep-2 dx dy)
                         do (info "dx=~a, dy=~a, shifted=~a~%"
                                  dx dy shifted-rep-2)
                            (when (= (length
                                      (set-intersection rep-1 shifted-rep-2))
                                     total)
                              (return-from outer shifted-rep-2))))
      )))


(defun rot-overlaps-2d (rep-1 rep-2 total)
  "Check if TOTAL points in REP-1 and REP-2 reports overlap considering move and
rotation."
  ;; check all rep-2 rotations for overlap with rep-1
  (loop for rot-rep-2 in (rep-rotations2d rep-2)
        for found-overlap = (progn
                              (info "rotation: ~a~%" rot-rep-2)
                              (overlaps2d rep-1 rot-rep-2 total))
        when found-overlap
          return found-overlap))

(defun rot-overlaps-2d (rep-1 rep-2 total)
  "Check if TOTAL points in REP-1 and REP-2 reports overlap considering move and
rotation."
  ;; check all rep-2 rotations for overlap with rep-1
  (loop for rot-rep-2 in (rep-rotations2d rep-2)
        for found-overlap = (progn
                              (info "rotation: ~a~%" rot-rep-2)
                              (overlaps2d rep-1 rot-rep-2 total))
        when found-overlap
          return found-overlap))
  
;; so what do we do for part one?
;; we take one scanner and find a pair with intersected 12 beacons among others

;; now the same but for 3d

(defun overlaps3d (rep-1 rep-2 total)
  "Checks if we can find TOTAL overlapping points between reports REP-1 and 
REP-2. Coordinate system is shifted between two reports."
  ;; let us find out borders of both reports and then try to move right report
  ;; and check how much overlaps
  ;; we start by setting top-right corner of rep2 to bottom-left corner of rep1
  ;; and then we move rep2 right and up, row by row
  ;; find coordinates of rep1's bottom-left corner 
  (labels ((borders (report)
             (info "borders: ~a~%" report)
             (loop for (x y z) in report
                   minimize x into min-x 
                   minimize y into min-y
                   minimize z into min-z
                   maximize x into max-x
                   maximize y into max-y
                   maximize z into max-z
                   finally (return (list
                                    (list min-x min-y min-z)
                                    (list max-x max-y max-z)))))
           (offset (report dx dy dz)
             (loop for (x y z) in report
                   collect (list (+ x dx) (+ y dy) (+ z dz)))))
    ;; we take one random point of rep-2 and put it in place of rep-1 points
    (loop named outer
          with counter = 0
          for p1 in rep-1 do
            (loop for p2 in rep-2
                  for dx = (- (car p1) (car p2))
                  for dy = (- (cadr p1) (cadr p2))
                  for dz = (- (caddr p1) (caddr p2))
                  for shifted-rep-2 = (offset rep-2 dx dy dz)
                  for overlaps = (set-intersection rep-1 shifted-rep-2)
                  do ;; (incf counter)
                     ;; (info "Next dx=~a, dy=~a, dz=~a, count=~a~%"
                     ;;       dx dy dz counter)
                     (when (= (length overlaps) total)
                       (return-from outer overlaps))
          ))))

(defun rot-overlaps3d (rep-1 rep-2 total)
  "Check if TOTAL points in REP-1 and REP-2 reports overlap considering move and
rotation."
  ;; check all rep-2 rotations for overlap with rep-1
  (loop for rot-rep-2 in (rep-rotations3d rep-2)
        for found-overlap = (progn
                              ;; (info "rotation: ~a~%" rot-rep-2)
                              (overlaps3d rep-1 rot-rep-2 total))
        when found-overlap
          do (return found-overlap)))

(defun part1 (path)
  (let ((reports (read-input path)))
    (loop with all-beacons = (car reports)
          while reports
          for rep-1 = (pop reports)
          for matched = (loop for rep-2 in reports
                              for overlapped = (rot-overlaps3d rep-1 rep-2 12)
                              when overlapped
                                return overlapped)
          when matched
            do (setf all-beacons (append all-beacons matched))
               (setf matched (remove-duplicates matched :test #'equal))
          finally (format t "Beacons: ~a~%" all-beacons)
                  (format t "Total count: ~a~%" (length all-beacons))
                  (length all-beacons))))
