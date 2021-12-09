(defun split-by-space (s)
  (loop for i = 0 then (1+ j)
        as j = (position #\Space s :start i)
        collect (subseq s i j)
        while j))

(defun part1 (path)
  (let* ((lines (uiop:read-file-lines path)))
    (loop for line in lines
          for output-pos = (+ 2 (position #\| line))
          for output-value = (subseq line output-pos)
          for digits = (split-by-space output-value)
          for digit-lengths = (mapcar #'length digits)
          for count = (length (intersection digit-lengths '(2 4 3 7)))
          sum count)))

(defun to-chars (s) (coerce s 'list))
(defun to-chars-transformer (stream subchar arg)
  (declare (ignore subchar) (ignore arg))
  (let* ((s (read stream t)))
    `(to-chars ,s)))
(set-dispatch-macro-character #\# #\d #'to-chars-transformer)

(defparameter *original-digits*
  (list
   (cons 0 #d"abcefg")
   (cons 1 #d"cf")
   (cons 2 #d"acdeg")
   (cons 3 #d"acdfg")
   (cons 4 #d"bcdf")
   (cons 5 #d"abdfg")
   (cons 6 #d"abdefg")
   (cons 7 #d"acf")
   (cons 8 #d"abcdefg")
   (cons 9 #d"abcdfg")))

(defun set-equal (list1 list2)
  (and (= (length list1) (length list2))
       (every (lambda (x) (member x list2 :test #'equal)) list1)
       (every (lambda (x) (member x list1 :test #'equal)) list2)))

(defun remap (digit segment-mappings)
  (mapcar
   (lambda (c) (cdr (assoc c segment-mappings)))
   digit))

(defun mapping-fits-p (mapping patterns)
  ;; we check that if we remap every pattern according to mapping - we will get
  ;; 10 original digits
  (let* ((remapped (mapcar
                    (lambda (p) (sort (remap p mapping) #'char<))
                    patterns))
         (orig-digits (mapcar #'cdr *original-digits*)))
    (set-equal orig-digits remapped)))

(defparameter *original-segments* #d"abcdefg")

(defun permutations (xs)
  (cond ((null xs) nil)
        ((null (cdr xs)) (list xs))
        (t (loop for x in xs
                 append (mapcar (lambda (l) (cons x l))
                                (permutations (remove x xs)))))))

(defun find-mapping (digit-strings)
  "DIGIT-STRINGS is a list of 10 strings each one encoding a digit."
  (let* ((patterns (mapcar #'to-chars digit-strings))
         (permuted-segments (permutations *original-segments*)))
    ;; now for each mappings lets check if it fits
    (loop for permutation in permuted-segments
          for mapping = (mapcar #'cons permutation *original-segments*)
          when (mapping-fits-p mapping patterns)
            do (return mapping))))

(defun decode (digit mapping)
  (let* ((digit-segments 
           (sort (mapcar (lambda (c) (cdr (assoc c mapping))) digit) #'char<)))
    (car (rassoc digit-segments original-digits :test #'equal))))
  
(defun part2 (path)
  (let* ((lines (uiop:read-file-lines path)))
    (loop
      for line in lines
      sum
      (let* ((delim-pos (position #\| line))
             (input-strings (split-by-space (subseq line 0 (1- delim-pos))))
             (output-strings (split-by-space (subseq line (+ 2 delim-pos))))
             (mapping (find-mapping input-strings)))
        (format t "Found mapping: ~a~%" mapping)
        (let ((digits (mapcar (lambda (x) (decode (to-chars x) mapping)) output-strings)))
          (reduce (lambda (x y) (+ (* x 10) y)) digits))))))
