(defun bin (c)
  (ecase c
    (#\# 1)
    (#\. 0)))

(defun img (b)
  (ecase b
    (0 #\.)
    (1 #\#)))

(defun enlarge (image step &optional (fill 0))
  "Enlarges image matrix by STEP elements on each side. So for step 1 new matrix
will have dimensions (rows + 2) x (cols + 2). FILL determines bit value which 
fills new elements on the border."
  (let* ((rows (array-dimension image 0))
         (cols (array-dimension image 1))
         (enlarged-image (make-array (list
                                      (+ rows (* 2 step))
                                      (+ cols (* 2 step)))
                                     :initial-element fill
                                     :element-type 'bit)))
    (loop 
      for row from 0 below rows do
        (loop for col from 0 below cols do
          (setf (aref enlarged-image (+ row step) (+ col step))
                (aref image row col))))
    enlarged-image))

(defun shrink (image)
  "Removed borders of image. Final image will have dimensions 
(rows - 1) x (cols - 1)."
  (let* ((rows (array-dimension image 0))
         (cols (array-dimension image 1))
         (shrinked-image (make-array (list (- rows 2)
                                           (- cols 2))
                                     :element-type 'bit)))
    (loop for row from 1 below (1- rows) do
      (loop for col from 1 below (1- cols) do
        (setf (aref shrinked-image (1- row) (1- col))
              (aref image row col))))
    shrinked-image))

(defun read-input (path)
  "Returns vector with enhancement algorithm and matrix with image in CAR and 
CDR of result."
  (with-open-file (in path)
    (let ((algorithm
            (loop for line = (read-line in nil)
                  while (> (length line) 0)
                  append (map 'list #'bin line) into alg-chars
                  finally (return
                            (make-array (list (length alg-chars))
                                        :element-type 'bit
                                        :initial-contents alg-chars))))
          (image
            (loop for line = (read-line in nil)
                  while line
                  collect line into lines
                  finally (return
                            (make-array (list (length lines)
                                              (length (car lines)))
                                        :element-type 'bit
                                        :initial-contents
                                        (mapcar (lambda (x) (map 'list #'bin x))
                                                lines))))))
      (cons algorithm image))))

(defun print-image (image)
  (loop for i from 0 below (array-dimension image 0) do
    (loop for j from 0 below (array-dimension image 1) do
      (format t "~a" (img (aref image i j))))
    (format t "~%")))

(defun index-at (image row col)
  "Returns binary number encoded by 9 bits around ROWxCOL element of IMAGE."
  (loop with idx = 0 for i from (1- row) to (1+ row) do
    (loop for j from (1- col) to (1+ col) do
      (setf idx (+ (ash idx 1) (aref image i j))))
        finally (return idx)))

(defun enhance (image algorithm &optional (fill 0))
  "Performs one enhance step. Returns new enlarged image. Plane contains bit 
value for infinity plane."
  ;; 1. enlarge image by 3
  ;; 2. enhance it
  ;; 3. shrink it by 1 (remove outer border which was not modified)
  (let* ((rows (array-dimension image 0))
         (cols (array-dimension image 1))
         (enlarged-image (enlarge image 3 fill))
         (enhanced-image (make-array (array-dimensions enlarged-image)
                                     :element-type 'bit)))
    ;; (format t "Enlarged picture:~%")
    ;; (print-image enlarged-image)

    ;; now we fill enhanced image by algorithm
    (loop for row from 1 to (+ rows 4) do
      (loop for col from 1 to (+ cols 4)
            for idx = (index-at enlarged-image row col)
            do (setf (aref enhanced-image row col)
                     (aref algorithm idx))))
    ;; (format t "Enhanced picture:~%")
    ;; (print-image enhanced-image)
    (shrink enhanced-image)))

(defun part1 (path)
  (let* ((input (read-input path))
         (image (cdr input))
         (algorithm (car input))
         (fills (if (= 1 (bit algorithm 0))
                    '(0 1)
                    '(0 0)))
         (enhanced image))
    (dolist (fill fills)
      (setf enhanced (enhance enhanced algorithm fill)))
    (loop
      with result = 0
      for i from 0 below (array-dimension enhanced 0) do
        (loop for j from 0 below (array-dimension enhanced 1) 
              when (> (aref enhanced i j) 0)
                do (incf result))
      finally (format t "Final light pixels: ~a~%" result))))
