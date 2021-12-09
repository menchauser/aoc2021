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

(defparameter test-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
cdfeb fcadb cdfeb cdbaf")


(defun process-line (line)
  "We get line with patterns, #\| delimiter, and digits. Then do something about
it."
  (let* ((delimiter-pos (position #\| line))
         (patterns (split-by-space (subseq line 0 (1- delimiter-pos))))
         (pattern-lengths (mapcar (lambda (p) (cons (coerce p 'list) (length p))) patterns)))
    ;; let us find "a" substitution
    (let* ((digit-1 (car (rassoc 2 pattern-lengths)))
           (digit-4 (car (rassoc 4 pattern-lengths)))
           (digit-7 (car (rassoc 3 pattern-lengths)))
           (digit-8 (car (rassoc 7 pattern-lengths)))
           (segment-a (set-difference digit-7 digit-1))
           (segments-bd (set-difference digit-4 digit-1))
           (segments-aeg (set-difference digit-8 digit-4))
           (segments-eg (set-difference segments-aeg segment-a)))
      (format t "1 = ~a~%4 = ~a~%7 = ~a~%8 = ~a~%"
              digit-1 digit-4 digit-7 digit-8)
      (format t "segment a: ~a~%" segment-a)
      (format t "segments [bd]: ~a~%" segments-bd)
      (format t "segments [aeg]: ~a~%" segments-aeg)
      (format t "segments [eg]: ~a~%" segments-eg)
      )))

(defun to-chars (s) (coerce s 'list))

(defparameter original-digits
  (list
   (cons 0 (to-chars "abcefg"))
   (cons 1 (to-chars "cf"))
   (cons 2 (to-chars "acdeg"))
   (cons 3 (to-chars "acdfg"))
   (cons 4 (to-chars "bcdf"))
   (cons 5 (to-chars "abdfg"))
   (cons 6 (to-chars "abdefg"))
   (cons 7 (to-chars "acf"))
   (cons 8 (to-chars "abcdefg"))
   (cons 9 (to-chars "abcdfg"))))

(defun set-equal (list1 list2)
  (and (= (length list1) (length list2))
       (every (lambda (x) (member x list2)) list1)
       (every (lambda (x) (member x list1)) list2)))

(defun remap (digit segment-mappings)
  (mapcar
   (lambda (c) (cdr (assoc c segment-mappings)))
   digit))

(defun valid-permutation (segment-mappings digits)
  "Segment mappings: alist of original-segment . target-segment.
Digits: alist of number . pattern.
Example: (validate-permutation '((d . a) (e . b) ...) '((1 . (a b)))"
  ;; we remap digit according to segment mappings and compare it with original)
  ;;(format t "Segment mappings: ~a~%Digits: ~a~%" segment-mappings digits)
  (loop for (num . digit) in digits
        always (set-equal (cdr (assoc num original-digits))
                          (remap digit segment-mappings))))

(defparameter segments (to-chars "abcdefg"))

(defun permutations (xs)
  (cond ((null xs) nil)
        ((null (cdr xs)) (list xs))
        (t (loop for x in xs
                 append (mapcar (lambda (l) (cons x l))
                                (permutations (remove x xs)))))))

(defun zip-alist (list1 list2)
  (mapcar #'cons list1 list2))

(defun remove-from (list &rest items)
  (let ((result (copy-list list)))
    (dolist (i items)
      (setf result (delete i result :test #'equal)))
    result))

(defun find-3 (patterns digit-1)
  (find-if
   (lambda (p) (= 3 (length (set-difference p digit-1))))
   patterns))

(defun find-mapping (string-patterns)
  (let* ((patterns (mapcar (lambda (p) (to-chars p))
                           (split-by-space string-patterns)))
         (digit-1 (find-if (lambda (p) (= 2 (length p))) patterns))
         (digit-4 (find-if (lambda (p) (= 4 (length p))) patterns))
         (digit-7 (find-if (lambda (p) (= 3 (length p))) patterns))
         (digit-8 (find-if (lambda (p) (= 7 (length p))) patterns))
         (rem-patterns (remove-from patterns digit-1 digit-4 digit-7 digit-8))
         (digit-3 (find-3 rem-patterns digit-1))
         (mappings (remove (to-chars "abcdefg")
                           (permutations (to-chars "abcdefg"))
                           :test #'equal))
         (digits (mapcar #'cons '(1 3 4 7 8)
                         (list digit-1 digit-3 digit-4 digit-7 digit-8))))
    (format t "1 = ~a~%3 = ~a~%4 = ~a~%7 = ~a~%8 = ~a~%"
            digit-1 digit-3 digit-4 digit-7 digit-8)
    ;; (format t "remaining patterns: ~a~%" rem-patterns)
    (loop for mapping in mappings
          when (valid-permutation (mapcar #'cons mapping segments) digits)
            do (return mapping))))

(defun split-by-delimiter (line delim)
  (let* ((delim-pos (position delim line)))
    (cons
     (subseq line 0 delim-pos)
     (subseq line (1+ delim-pos)))))

(defun decode (digit mapping)
  (let* ((mlist (mapcar #'cons mapping (to-chars "abcdefg")))
         (digit-segments 
           (sort (mapcar (lambda (c) (cdr (assoc c mlist))) digit) #'char<)))
    (car (rassoc digit-segments original-digits :test #'equal))))

(defun part1 (path)
  (let* ((lines (uiop:read-file-lines path)))
    (dolist (line lines)
      (let* ((delim-pos (position #\| line))
             (string-patterns (subseq line 0 (1- delim-pos)))
             (string-digits (split-by-space (subseq line (+ 2 delim-pos))))
             (mapping (find-mapping string-patterns)))
        (dolist (d string-digits)
          (format t "~a = ~a~%" d (decode (to-chars d) mapping)))))))
             
