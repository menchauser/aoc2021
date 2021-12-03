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

(defun count-elements (xs)
	"Gets a list and returns alist of (element . count) where count is number of 
that element in string."
	(let ((counts nil))
		(dolist (x xs)
			(if (assoc x counts)
					(incf (cdr (assoc x counts)))
					(setf counts (cons (cons x 1) counts))))
		counts))

(defun max-element (xs &key (key< #'<) (value< #'<))
	"Gets an alist and finds maximum element according to element values. The 
values should be non-negative. If values are equal: element with bigger key is 
selected (so char #\1 > #\0)."
	(if (eql 1 (length xs))
			(car xs)
			(reduce
			 (lambda (x y)
				 (cond
					 ((funcall value< (cdr x) (cdr y)) y)
					 ((eql (cdr x) (cdr y)) (if (funcall key< (car x) (car y)) y x))
					 (t x)))
			 (cdr xs)
			 :initial-value (car xs))))

(defun most-common (strings position)
	"Returns character which is most common in the given position of passed 
strings"
	(let* ((chars (mapcar (lambda (s) (char s position)) strings))
				 (counts (count-elements chars)))
		(car (max-element counts :key< #'char<))))

(defun least-common (strings position)
	"Returns character which is least common in the given position of passed 
STRINGS."
	(let* ((chars (mapcar (lambda (s) (char s position)) strings))
				 (counts (count-elements chars)))
		(car (max-element counts :key< #'char> :value< #'>))))

(defun filter-strings (strings ch position)
	"Select only strings with the character CH in given POSITION."
	(remove-if-not
	 (lambda (str) (eql ch (char str position)))
	 strings))

(defun part2-loop (lines char-function position)
	;; if only one line remains - return it
	;; otherwise: filter lines by next position and make step
	(format nil "Filtering lines ~a position ~a~%" lines position)
	(cond
		((> position (length (car lines)))
		 (error "position is outside of string length"))
		((<= (length lines) 1) (car lines))
		(t
		 (let ((ch (funcall char-function lines position)))
			 (format nil "Selected character: ~a~%" ch)
			 (part2-loop
				(filter-strings lines ch position)
				char-function
				(1+ position))))))

(defun part2-max-loop (lines)
	(part2-loop lines #'most-common 0))
		
(defun part2-min-loop (lines)
	(part2-loop lines #'least-common 0))

;; Now we start working the list of symbols until either the string ends or only
;; one string remains
(defun part2 (path)
	(let* ((lines (uiop:read-file-lines path))
				 (oxygen-value (parse-integer (part2-max-loop lines) :radix 2))
				 (co2-value (parse-integer (part2-min-loop lines) :radix 2)))
		(* oxygen-value co2-value)))
