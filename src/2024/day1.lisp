(defpackage cl-aoc/2024/day1
  (:use :cl)
  (:import-from :str)
  (:import-from :parse-number #:parse-number)
  (:export #:1a #:1b))
(in-package :cl-aoc/2024/day1)

;; https://adventofcode.com/2024/day/1

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1a

(defun 1a (input)
  (loop :for a :in (car input)
        :for b :in (cadr input)
        :summing (if (< a b)
                     (- b a)
                     (- a b)) into total
        :finally (return total)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1b

(defun 1b ()
  (print "1b"))
