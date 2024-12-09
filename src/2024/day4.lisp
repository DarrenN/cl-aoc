(defpackage cl-aoc/2024/day4
  (:use :cl)
  (:import-from :str)
  (:import-from :alexandria #:flatten)
  (:import-from :serapeum #:dict)
  (:import-from :parse-number #:parse-number)
  (:local-nicknames (#:i #:iterate))
  (:export #:part1 #:part2))
(in-package :cl-aoc/2024/day4)

;;; AoC Day 4
;;; https://adventofcode.com/2024/day/4
;;;
;;; Note: I'm in the process of learning Common Lisp while doing these puzzles.
;;;
;;; Sigificant assistance from steveo:
;;; https://gist.github.com/saolsen/89773cc5812a5ad28f57f871f596a143

(defparameter *ti* "../../input/day4-test.txt")
(defparameter *in* "../../input/day4.txt")

(defparameter *dir* #2A((1 1)
                        (-1 1)
                        (1 0)
                        (0 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part1

(defun get-char (y x matrix)
  "Safely extract a char from matrix repsecting the bounds."
  (cond
    ((< y 0) '())
    ((> y (1- (array-dimension matrix 0))) '())
    ((< x 0) '())
    ((> x (1- (array-dimension matrix 1))) '())
    (t (aref matrix y x))))

(defun get-neighbors (&key y x matrix)
  "For each X scan the neighboring 3 cells for MAS in every direction."
  (i:iter outer (i:for i below (array-dimension *dir* 0))
    (i:for dx = (aref *dir* i 0))
    (i:for dy = (aref *dir* i 1))
    (when (or
           (and (eq (get-char (+ y dy) (+ x dx) matrix) #\M)
                (eq (get-char (+ y (* 2 dy)) (+ x (* 2 dx)) matrix) #\A)
                (eq (get-char (+ y (* 3 dy)) (+ x (* 3 dx)) matrix) #\S))
           (and (eq (get-char (- y dy) (- x dx) matrix) #\M)
                (eq (get-char (- y (* 2 dy)) (- x (* 2 dx)) matrix) #\A)
                (eq (get-char (- y (* 3 dy)) (- x (* 3 dx)) matrix) #\S)))
      (i:sum 1))))

(defun part1 (filename)
  (let* ((lines (uiop:read-file-lines filename))
         (matrix
           (make-array (list (length lines) (length (coerce (car lines) 'list)))
                       :initial-element #\.))
         (dim (array-dimensions matrix)))

    ;; Load the matrix (mutation)
    (loop :for line :in lines
          :for y :from 0 :to (car dim)
          :do
             (loop :for c :across line
                   :for x :from 0 :to (cadr dim)
                   :do (setf (aref matrix y x) c)))

    ;; Scan the matrix
    (i:iter outer (i:for i below (array-dimension matrix 0))
      (i:iter (i:for j below (array-dimension matrix 1))
        (i:in outer
              (when (eq #\X (aref matrix i j))
                (i:sum (get-neighbors :y i :x j :matrix matrix))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part2

(defun part2 (filename)
  filename)
