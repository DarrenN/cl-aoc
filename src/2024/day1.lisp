(defpackage cl-aoc/2024/day1
  (:use :cl)
  (:import-from :str)
  (:import-from :serapeum #:dict)
  (:import-from :parse-number #:parse-number)
  (:export #:1a #:1b))
(in-package :cl-aoc/2024/day1)

;;; AoC Day 1
;;; https://adventofcode.com/2024/day/1
;;;
;;; Note: I'm in the process of learning Common Lisp while doing these puzzles.
;;;
;;; Broke each answer into an initial step reading the file and converting into
;;; the appropriate data structures. Then in the 1a and 1b functions loop over
;;; those data structures to sum up the answers. Really leaned into the loop
;;; macro to do the summing.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1a

(defun process-input (filename)
  "Convert input into two lists, sorted by <"
  (let* ((ls (uiop:read-file-lines filename))
         (ns
           (reduce
            (lambda (acc l)
              (let ((s (str:split " " l :omit-nulls t)))
                `(,(append (list (parse-number (car s))) (car acc))
                  ,(append (list (parse-number (cadr s))) (cadr acc)))))
            ls :initial-value '(() ()))))
    `(,(sort (copy-seq (car ns)) #'<)
      ,(sort (copy-seq (cadr ns)) #'<))))

(defun 1a (filename)
  "Loop over the two lists subtracting left from right, and summing that value."
  (let ((input (process-input filename)))
    (loop :for a :in (car input)
          :for b :in (cadr input)
          :summing (abs (- a b)) into total
          :finally (return total))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1b

(defun process-input2 (filename)
  "Convert input into a hash-table keyed on right values storing a frequency
count of right values seen. Also returns a list of the left values which we use
to pull values from the hash-table."
  (let* ((ls (uiop:read-file-lines filename))
         (h (dict))
         (ns
           (reduce
            (lambda (acc l)
              (let* ((s (str:split " " l :omit-nulls t))
                     (l (parse-number (car s)))
                     (r (parse-number (cadr s)))
                     (v (gethash r h)))
                (if v
                    (setf (gethash r h) (+ v 1))
                    (setf (gethash r h) 1))
                (append `(,l) acc)))
            ls :initial-value '())))
    (values h ns)))

(defun 1b (filename)
  "Loop over the left values, extracting the frequency count from hash-table,
multiplying left value by frequency count, and summing."
  (multiple-value-bind (h ls)
      (process-input2 filename)
    (loop :for l :in ls
          :summing (if (gethash l h)
                       (* (gethash l h) l)
                       0) into total
          :finally (return total))))
