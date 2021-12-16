(defun to-binary (hex-str)
  (format nil "~b" (parse-integer hex-str :radix 16)))

(defun bin (bin-str)
  (parse-integer bin-str :radix 2))


(defun read-literal (bits &key start)
  "START position is set to the beginning of first group of literal bits. 
Returns literal value and position after the end."
  ;; lets read next 5 bits
  ;;(format t "~a~%" bits)
  ;;(format t "~v@{~a~:*~}" start #\Space)
  (loop with tag = #\A
        with value = 0
        with pos = start
        for group-bit = (bin (subseq bits pos (1+ pos)))
        for group = (subseq bits (1+ pos) (+ 5 pos))
        do (format t "~v@{~a~:*~}" 5 tag)
           (setf value (+ (* value 16) (bin group))
                 pos (+ pos 5)
                 tag (code-char (1+ (char-code tag))))
        if (= 0 group-bit)
          ;; count final zeroes to add to pos
          return (loop for p from pos below (length bits)
                       while (eql (elt bits p) #\0)
                       do (format t "-")
                       finally (return (values value p)))))
  
(defun read-packet (bits &key (start 0))
  (format t "~&~a~%" (subseq bits start))
  (format t "VVVTTT")
  (let ((version (bin (subseq bits start (+ start 3))))
        (type (bin (subseq bits (+ start 3) (+ start 6)))))
    ;; we have version and type
    ;; now we have to determine where packet ends
    ;; for literal value we read groups of 5 bits until we reach one with 0
    ;; for operator
    (case type
      (4
       (multiple-value-bind (value pos)
           (read-literal bits :start (+ start 6))
         (values (list 'literal version type value)
                 pos)))
      (otherwise
       (let ((length-type (bin (subseq bits (+ start 6) (+ start 7)))))
         (format t "I")
         (case length-type
           ;; next 15 bits - total length
           (0
            (format t "~v@{~a~:*~}" 15 "L")
            (let ((total-length (bin (subseq bits (+ start 7) (+ start 7 15)))))
              ;; (format t "~&start position: ~a, end:~a~%"
              ;;         (+ start 7 15) (+ start 7 15 total-length))
              (loop with pos = (+ start 7 15)
                    while (< pos (+ start 7 15 total-length))
                    for (subpacket next-pos) = (multiple-value-list
                                                (read-packet bits :start pos))
                    do (setf pos next-pos)
                    collect subpacket into subpackets
                    finally (return (values (list 'op
                                                  version
                                                  type
                                                  length-type
                                                  total-length
                                                  subpackets)
                                            pos)))))
           ;; next 11 bits - number of sub-packets 
           (1
            (format t "~v@{~a~:*~}" 11 "L")
            (let ((total-count (bin (subseq bits (+ start 7) (+ start 7 11)))))
              (loop with pos = (+ start 7 11)
                    repeat total-count
                    for (subpacket next-pos) = (multiple-value-list
                                                (read-packet bits :start pos))
                    do (setf pos next-pos)
                    collect subpacket into subpackets
                    finally (return (values (list 'op
                                                  version
                                                  type
                                                  length-type
                                                  total-count
                                                  subpackets)
                                            pos)))))))))))
