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

(defun read-literal-value (bits &key start (finalize nil))
  "START position is set to the beginning of first group of literal bits. 
FINALIZE demands to consume zeroes after the final group. Returns literal value 
and position after the end."
  ;;(format t "~a~%" bits)
  ;;(format t "~v@{~a~:*~}" start #\Space)
  (loop with tag = #\A
        with value = 0
        with pos = start
        for group-bit = (bin (subseq bits pos (1+ pos)))
        for group = (subseq bits (1+ pos) (+ 5 pos))
        do ;; (format t "~v@{~a~:*~}" 5 tag)
           (setf value (+ (* value 16) (bin group))
                 pos (+ pos 5)
                 tag (code-char (1+ (char-code tag))))
        if (= 0 group-bit)
          ;; (- pos start) should be divisible by 4
          ;; count final zeroes to add to pos
          return (if finalize 
                     (let ((pos-mod (mod pos 4)))
                       (case pos-mod
                         (0 (values value pos))
                         (otherwise (loop repeat (- 4 pos-mod)
                                          do (format t "-")
                                             (incf pos)
                                          finally (return (values value pos))))))
                     (values value pos))))
  
(defun read-packet (bits &key (start 0))
  ;; (format t "~&~a~%" (subseq bits start))
  ;; (format t "VVVTTT")
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
         ;; (format t "I")
         (case length-type
           ;; next 15 bits - total length
           (0
            ;; (format t "~v@{~a~:*~}" 15 "L")
            (let ((total-length (bin (subseq bits (+ start 7) (+ start 7 15)))))
              ;; (format t "~&start position: ~a, end:~a~%"
              ;;         (+ start 7 15) (+ start 7 15 total-length))
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
            ;; (format t "~v@{~a~:*~}" 11 "L")
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

(defun part1 (path)
  (let* ((line (uiop:read-file-line path))
         (packet (read-packet (to-binary line))))
    (sum-versions packet)))
