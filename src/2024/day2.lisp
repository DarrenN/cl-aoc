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

(defun part1 (filename)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part2

(defun sliding-window2 (lst window-size)
  "Generate a list of sliding windows of size WINDOW-SIZE from LST."
  (let ((up 0)
        (down 0)
        (zero 0))
    (when (and lst (> window-size 0))
      (loop for i from 0 to (- (length lst) window-size)
            do (let* ((w (subseq lst i (+ i window-size)))
                      (s (- (car w) (cadr w))))
                 (cond
                   ((= s 0) (setf zero (+ zero 1)))
                   ((> s 0) (setf up (+ up 1)))
                   ((< s 0) (setf down (+ down 1)))
                   (t (print s))))))
    ;(print (format nil "~a ~a ~a ~a" up down zero lst))
    (cond
      ((> zero 1) 0)
      ((= up down) 0)
      ((and (> down up) (> up 1)) 0)
      ((and (> up down) (> down 1)) 0)
      (t 1))))

(defun remove-nth (idx lst)
  (when (and lst (>= (length lst) idx))
    (loop for i from 0 to (- (length lst) 1)
          when (not (= i idx))
          collect (nth i lst))))

(defun sliding-window3 (lst)
  "Generate a list of sliding windows of size WINDOW-SIZE from LST."
  (let ((dir 0)
        (curr 0)
        (next 0)
        (bad 0))
    (when (and lst (> (length lst) 2))
      (loop for i from 0 to (- (length lst) 1)
            do (let* ((c (nth i lst))
                      (n (if (< i (- (length lst) 2))
                             (nth (+ i 1) lst)
                             (nth i lst))))
                 (when (= dir 0)
                   (setf dir (- c n)))
                 (when (= bad 0)
                   (cond
                     ((= c n) (setf bad i))
                     ((and (< dir 0) (> (- c n) 0)) (setf bad i))
                     ((and (> dir 0) (< (- c n) 0)) (setf bad i))
                     (t (setf dir (- c n)))))
                 (print (format nil "~a ~a ~a ~a ~a" dir curr next bad lst))
                 )))
    (if (> bad 0)
        (remove-nth bad lst)
        lst)))

(defun part2 (filename)
  (let* ((ls (uiop:read-file-lines filename))
         (reports
           (reduce
            (lambda (acc rs)
              (let* ((r (loop :for s :in (str:split " " rs :omit-nulls t)
                              :collect (parse-number s)))
                     (v (or (apply #'< r) (apply #'> r))))
                ;; now check any v = T reports for inc/dec within bounds
                (if v
                    (+ (sliding-window r 2) acc)
                    (let ((r2 (sliding-window3 r)))
                      ;(print r2)
                      (if (or (apply #'< r2) (apply #'> r2))
                          (+ (sliding-window r2 2) acc)
                          acc)))))
            ls :initial-value 0)))
    reports))
