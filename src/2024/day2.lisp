(defpackage cl-aoc/2024/day1
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

(defun process-input (filename)
  (let* ((ls (uiop:read-file-lines filename)))
    ls))

(defun part1 (filename)
  filename)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part2

(defun part2 (filename)
  filename)
