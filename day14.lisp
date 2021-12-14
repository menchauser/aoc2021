(defun read-input (path)
  "Returns template string as first value and list of CONSes of insertion rules 
as second."
  (with-open-file (in path)
    (let ((template (read-line in))
          (rules (loop for line = (read-line in nil)
                       while line
                       if (> (length line) 0)
                         collect (cons (subseq line 0 2)
                                       (subseq line 6)))))
      (values template rules))))

(defun insert (pair elem)
  (concatenate 'string (subseq pair 0 1) elem (subseq pair 1 2)))

(defun join-strings (&rest ss)
  ;; for all elements except first one we copy only 2-3 characters
  (concatenate 'string ss))

(defun grow-step (template rules)
  (loop for i from 0 to (- (length template) 2)
        for pair = (subseq template i (+ i 2))
        for rule = (assoc pair rules :test #'equal)
        if rule
          collect (progn
                    ;;(format t "rule: ~a~%" rule)
                    (insert (car rule) (cdr rule))) into result
        finally (return
                  (progn
                    ;;(format t "result: ~a~%" result)
                    (reduce (lambda (a b) (concatenate 'string a b))
                            (cons (car result)
                                  (mapcar (lambda (s) (subseq s 1))
                                          (cdr result))))))))

(defun naive-solution (path steps)
  (multiple-value-bind (template rules)
      (read-input path)
    (dotimes (i steps)
      (setf template (grow-step template rules)))
    ;; now count elements
    (loop with counts = (make-hash-table)
          for c across template
          do (incf (gethash c counts 0))
          finally (loop with min-key = (char template 0)
                        with min-val = (gethash min-key counts)
                        with max-key = min-key
                        with max-val = min-val
                        for k being the hash-keys of counts
                        for v = (gethash k counts)
                        when (> v max-val)
                          do (setf max-key k max-val v)
                        when (< v min-val)
                          do (setf min-key k min-val v)
                        finally
                           (progn
                             (format t "Min key=~a, count=~a~%" min-key min-val)
                             (format t "Max key=~a, count=~a~%" max-key max-val)
                             (format t "Result: ~a~%" (- max-val min-val)))))))

(defun print-hash (ht)
  (loop for k being the hash-keys in ht
        for v = (gethash k ht)
        do (format t "~a: ~a~%" k v)))

(defun solution (path steps)
  ;; we have finite number of possible pairs
  ;; we store two maps:
  ;; - counts of all possible chars
  ;; - counts of all existing pairs
  ;; on each step we:
  ;; for each pair that have count > 0
  ;;   produce new chars and increase their count in char-map
  ;;   if there are new pairs: decrease old pair count and increase new ones
  (multiple-value-bind (template rules)
      (read-input path)
    (let ((char-counts (make-hash-table))
          (pair-gens (make-hash-table :test #'equal))
          (pair-counts (make-hash-table :test #'equal)))
      ;; prepare pair-gen map
      (loop for (elems . new-elem) in rules
            do (setf (gethash elems pair-gens)
                     (list
                      new-elem
                      (concatenate 'string (subseq elems 0 1) new-elem)
                      (concatenate 'string new-elem (subseq elems 1)))))
      ;; prepare initial counts
      (loop for c across template do (incf (gethash c char-counts 0)))
      ;; prepare initial pairs
      (loop for k being the hash-keys of pair-gens
            for v = (gethash k pair-gens)
            do (setf (gethash k pair-counts) 0
                     (gethash (cadr v) pair-counts) 0
                     (gethash (caddr v) pair-counts) 0))
      (loop for i from 0 below (1- (length template))
            for pair = (subseq template i (+ i 2))
            do (incf (gethash pair pair-counts 0)))
      ;; data prepared. now go through steps
      (dotimes (i steps)
        (loop for k being the hash-keys of pair-counts
              for v = (gethash k pair-counts)
              with new-counts = nil
              when (> v 0)
                do (let* ((gen (gethash k pair-gens))
                          (gen-char (char (car gen) 0))
                          (gen-pairs (cdr gen)))
                     (incf (gethash gen-char char-counts 0) v)
                     (push (cons k (- v)) new-counts)
                     (dolist (gp gen-pairs)
                       (push (cons gp v) new-counts)))
              finally
                 (loop for (k . v) in new-counts
                       do (incf (gethash k pair-counts) v))))
      ;; (print-hash char-counts)
      (loop for v being the hash-values of char-counts
            maximize v into max-v
            minimize v into min-v
            finally (return (- max-v min-v))))))
