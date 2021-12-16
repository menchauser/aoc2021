(defparameter +debug+ t)

(defmacro info (control-string &rest args)
  `(when +debug+
     (format t ,control-string ,@args)))

(defun to-binary (hex-str)
  (loop for c across hex-str
        collect (format nil "~4,'0b" (parse-integer (string c) :radix 16))
          into parts
        finally (return (reduce (lambda (a b) (concatenate 'string a b)) parts))))

(defun bin (bin-str)
  (parse-integer bin-str :radix 2))

(defstruct literal
  version value)

(defstruct op
  version type length-type length subpackets)

(defun read-literal-value (bits &key start)
  "START position is set to the beginning of first group of literal bits. 
Returns literal value and position after the end."
  (loop with tag = #\A
        with value = 0
        with pos = start
        for group-bit = (bin (subseq bits pos (1+ pos)))
        for group = (subseq bits (1+ pos) (+ 5 pos))
        do (info "~v@{~a~:*~}" 5 tag)
           (setf value (+ (* value 16) (bin group))
                 pos (+ pos 5)
                 tag (code-char (1+ (char-code tag))))
        if (= 0 group-bit)
          return (values value pos)))
  
(defun read-packet (bits &key (start 0) (print-packet nil))
  (when print-packet
    (info "~&~a~%" bits))
  (info "VVVTTT")
  (let ((version (bin (subseq bits start (+ start 3))))
        (type (bin (subseq bits (+ start 3) (+ start 6)))))
    (case type
      (4
       (multiple-value-bind (value pos)
           (read-literal-value bits :start (+ start 6))
         (values (make-literal :version version :value value)
                 pos)))
      (otherwise
       (let ((length-type (bin (subseq bits (+ start 6) (+ start 7)))))
         (info "I")
         (case length-type
           ;; next 15 bits - total length
           (0
            (info "~v@{~a~:*~}" 15 "L")
            (let ((total-length (bin (subseq bits (+ start 7) (+ start 7 15)))))
              (loop with pos = (+ start 7 15)
                    while (< pos (+ start 7 15 total-length))
                    for (subpacket next-pos) = (multiple-value-list
                                                (read-packet bits :start pos))
                    do (setf pos next-pos)
                    collect subpacket into subpackets
                    finally (return (values (make-op
                                             :version version
                                             :type type
                                             :length-type length-type
                                             :length total-length
                                             :subpackets subpackets)
                                            pos)))))
           ;; next 11 bits - number of sub-packets 
           (1
            (info "~v@{~a~:*~}" 11 "L")
            (let ((total-count (bin (subseq bits (+ start 7) (+ start 7 11)))))
              (loop with pos = (+ start 7 11)
                    repeat total-count
                    for (subpacket next-pos) = (multiple-value-list
                                                (read-packet bits :start pos))
                    do (setf pos next-pos)
                    collect subpacket into subpackets
                    finally (return (values (make-op
                                             :version version
                                             :type type
                                             :length-type length-type
                                             :length total-count
                                             :subpackets subpackets)
                                            pos)))))))))))

(defun sum-versions (packet)
  (cond
    ((literal-p packet) (literal-version packet))
    (t (+ (op-version packet)
          (loop for sub in (op-subpackets packet)
                sum (sum-versions sub))))))

(defun bool-to-int (b)
  (if b 1 0))

(defun evaluate (packet)
  (if (literal-p packet)
      (literal-value packet)
      (let ((args (mapcar #'evaluate (op-subpackets packet))))
        (ecase (op-type packet)
          (0 (apply #'+ args))
          (1 (apply #'* args))
          (2 (apply #'min args))
          (3 (apply #'max args))
          (5 (bool-to-int (apply #'> args)))
          (6 (bool-to-int (apply #'< args)))
          (7 (bool-to-int (apply #'= args)))))))

(defun part1 (path)
  (let ((+debug+ nil))
    (sum-versions (read-packet (to-binary (uiop:read-file-line path))))))

(defun part2 (path)
  (let ((+debug+ nil))
    (evaluate (read-packet (to-binary (uiop:read-file-line path))))))
