(defpackage cl-aoc/2024/day2
  (:use :cl)
  (:import-from :str)
  (:import-from :serapeum #:dict)
  (:import-from :parse-number #:parse-number)
  (:export #:part1 #:part2))
(in-package :cl-aoc/2024/day2)

;;; AoC Day 2
;;; https://adventofcode.com/2024/day/2
;;;
;;; Note: I'm in the process of learning Common Lisp while doing these puzzles.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part1

(defun sliding-window (lst window-size)
  "Generate a list of sliding windows of size WINDOW-SIZE from LST."
  (let ((result 1))
    (when (and lst (> window-size 0))
      (loop for i from 0 to (- (length lst) window-size)
            do (let* ((w (subseq lst i (+ i window-size)))
                      (s (abs (- (car w) (cadr w)))))
                 (if (and (< s 4) (> s 0))
                     result
                     (setf result 0)))))
    result))

(defun process-input (filename)
  (let* ((ls (uiop:read-file-lines filename))
         (reports
           (reduce
            (lambda (acc rs)
              (let* ((r (loop :for s :in (str:split " " rs :omit-nulls t)
                              :collect (parse-number s)))
                     (v (or (apply #'< r) (apply #'> r))))
                ;; now check any v = T reports for inc/dec within bounds
                (if v
                    (+ acc (sliding-window r 2))
                    acc)))
            ls :initial-value 0)))
    reports))

(defun part1 (filename)
  (process-input filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part2

(defun part2 (filename)
  filename)
