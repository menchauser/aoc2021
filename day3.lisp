(defun digits (str)
	"Convert string representation of binary number to list of digits.
\"01011\" -> (0 1 0 1 1). Behavior is unexpected for non-number characters."
	(mapcar (lambda (c) (- (char-int c) (char-int #\0)))
					(coerce str 'list)))

(defun sum (xs ys)
	(mapcar #'+ xs ys))

(defun calc-bit (num threshold)
	(if (> num threshold) 1 0))

(defun complement-list (bs)
	"(0 1 0) -> (1 0 1)"
	(mapcar (lambda (x) (ecase x
												(0 1)
												(1 0)))
					bs))

(defun to-number (digits)
	"(1 0 1 1 0) -> 22."
	(parse-integer (coerce (mapcar #'digit-char digits) 'string)
								 :radix 2))

;; In: ("00100" "11110" "10110")
;; Out: (1 2 0 1 3)
(defun part1 (path)
	"Returns list of counts of zeros in each position of array. For input 
(\"001\" \"111\" \"101\") it returns (1 2 0)"
	;; Here we actually count number of 1s and total length
	(let* ((lines (uiop:read-file-lines path))
				 (digits-list (mapcar #'digits lines))
				 (counts-of-1 (reduce #'sum digits-list))
				 (half (/ (length digits-list) 2))
				 (gamma-digits (mapcar (lambda (x) (calc-bit x half)) counts-of-1))
				 (epsilon-digits (complement-list gamma-digits))
				 (gamma (to-number gamma-digits))
				 (epsilon (to-number epsilon-digits)))
		(* gamma epsilon)))
				 
		
		
