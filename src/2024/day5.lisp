(defpackage cl-aoc/2024/day5
  (:use :cl)
  (:import-from :str)
  (:import-from :serapeum #:dict)
  (:import-from :parse-number #:parse-number)
  (:local-nicknames (#:i #:iterate))
  (:local-nicknames (#:rx #:cl-ppcre))
  (:export #:part1 #:part2))

(in-package :cl-aoc/2024/day5)

;;; AoC Day 5
;;; https://adventofcode.com/2024/day/5
;;;
;;; Note: I'm in the process of learning Common Lisp while doing these puzzles.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse file

(defparameter *ti* "../../input/day5-test.txt")
(defparameter *in* "../../input/day5.txt")

(defun parse-input (filename)
  "Convert input into a hash-table of :orders and :updates.
:orders is a hash-table of page ordering rules.
:updates is a list of lists denoting update order."
  (let* ((lines (uiop:read-file-lines filename))
         (orderptrn (rx:create-scanner "\\d*|\\d*"))
         (section nil))
    (reduce
     (lambda (acc line)
       (let ((orders (gethash :orders acc))
             (updates (gethash :updates acc)))
         (cond
           ((equal line "") (setf section t))
           (section
            (setf (gethash :updates acc)
                  (append updates
                          (list
                           (mapcar #'parse-number
                                   (rx:split "," line))))))
           (t
            (let* ((vs (mapcar #'parse-number (rx:split "\\|" line)))
                   (k (car vs))
                   (v (cadr vs))
                   (os (gethash k orders)))
              (setf (gethash k orders)
                    (if os
                        (append os (list v))
                        (list v))))))
         acc))
     lines :initial-value (dict :orders (dict) :updates '()))))

(defun correct-order (updates orders)
  "Correct order:"
  (let* ((p (car updates))
         (hits
           (i:iter (i:for n in (cdr updates))
             (i:for us = (gethash p orders))
             (when (member n us)
               (i:sum 1))
             (setf p n))))
    (if (eq (length updates) (1+ hits))
        (car (nthcdr (floor (/ (length updates) 2)) updates))
        0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part1

(defun part1 (filename)
  (let* ((input (parse-input filename))
         (orders (gethash :orders input))
         (updates (gethash :updates input)))
    (i:iter (i:for u in updates)
      (i:sum (correct-order u orders)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part2

(defun part2 (filename)
  filename)
