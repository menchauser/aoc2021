(defun sum (xs)
  (reduce #'+ xs))

(defun count-increases (xs)
	"Count the number of increases between previous and current list element"
  (let ((diffs (mapcar #'- (cdr xs) xs)))
    (count-if #'plusp diffs)))

(defun group-by (xs n)
  "Groups list in sublists with n-elements each"
  (loop for i from 0 to (- (length xs) n) by 1
        collect (subseq xs i (+ i n))))


(defun part1 (path)
  (let* ((lines (uiop:read-file-lines path))
         (numbers (mapcar #'parse-integer lines)))
    (count-increases numbers)))

(defun part2 (path)
  (let* ((lines (uiop:read-file-lines path))
         (numbers (mapcar #'parse-integer lines))
         (groups (group-by numbers 3))
         (measurements (mapcar #'sum groups)))
    (count-increases measurements)))

