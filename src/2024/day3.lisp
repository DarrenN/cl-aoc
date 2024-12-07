(defpackage cl-aoc/2024/day3
  (:use :cl)
  (:import-from :str)
  (:import-from :trivia)
  (:import-from :serapeum #:dict)
  (:import-from :parse-number #:parse-number)
  (:export #:part1 #:part2))
(in-package :cl-aoc/2024/day3)

;;; AoC Day 3
;;; https://adventofcode.com/2024/day/3
;;;
;;; Note: I'm in the process of learning Common Lisp while doing these puzzles.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part1

(defun match-mul (c)
  (or (eq #\m c)
      (eq #\u c)
      (eq #\l c)
      (eq #\( c)))

(defun match-switch (c)
  (or (eq #\d c)
      (eq #\o c)
      (eq #\n c)
      (eq #\' c)
      (eq #\t c)
      (eq #\( c)
      (eq #\) c)))

(defun match-num (c)
  (or (eq #\0 c)
      (eq #\1 c)
      (eq #\2 c)
      (eq #\3 c)
      (eq #\4 c)
      (eq #\5 c)
      (eq #\6 c)
      (eq #\7 c)
      (eq #\8 c)
      (eq #\9 c)))

(defun clear-state (h)
  (loop :for k :in '(:state :param1 :param2)
        :do (setf (gethash k h) '()))
  h)

(defun handle-mul (h c)
  (let* ((s (gethash :state h))
         (r (append s `(,c))))
    (cond
      ((and (gethash :switch h)
            (equal s '(#\m #\u #\l))
            (equal c #\())
       (setf (gethash :state h) '(*)))
      ((or (equal r '(#\m #\u #\l))
           (equal r '(#\m #\u))
           (equal r '(#\m)))
       (setf (gethash :state h) r))
      (t (clear-state h)))))

(defun handle-num (h c)
  (let ((s (gethash :state h))
        (p1 (gethash :param1 h))
        (p2 (gethash :param2 h)))
    (cond
      ((and (equal s '(*))
            (<= (length p1) 3))
       (setf (gethash :param1 h) (append (gethash :param1 h) `(,c))))
      ((and (equal s '(#\,))
            (<= (length p2) 3))
       (setf (gethash :param2 h) (append (gethash :param2 h) `(,c))))
      (t (clear-state h)))))

(defun handle-comma (h c)
  (let ((s (gethash :state h))
        (p1 (gethash :param1 h)))
    (if (and (equal s '(*))
             (<= (length p1) 3))
        (setf (gethash :state h) '(#\,))
        (clear-state h))))

(defun calc-prods (h)
  (let ((p1 (gethash :param1 h))
        (p2 (gethash :param2 h)))
    (* (parse-number (coerce p1 'string))
       (parse-number (coerce p2 'string)))))

(defun handle-close (h c)
  (let ((s (gethash :state h))
        (p1 (gethash :param1 h))
        (p2 (gethash :param2 h)))
    (cond
      ((and (gethash :switch h)
            (equal s '(#\,))
            (<= (length p1) 3)
            (<= (length p2) 3))
       (prog2
           (setf (gethash :prods h) (append (gethash :prods h) `(,(calc-prods h))))
           (clear-state h)))
      ((and (equal s '(#\d #\o #\())
            (equal c #\)))
       (setf (gethash :switch h) t))
      ((and (equal s '(#\d #\o #\n #\' #\t #\())
            (equal c #\)))
       (setf (gethash :switch h) '()))
      (t (clear-state h)))))

  ;; if state = (list #\m #\u #\l #\() set to '(*)
  ;; if state = '* and c is match-num and param1 <= 3 then add to param1
  ;; if state = , and c is match-num and param2 <= 3 then add to param2
  ;; if state = ) and param1 and param2 are at least 1 the add (* p1 p2) to prods
  ;; else clear state and param1 param2

(defun handle-char (h c)
  ;(print (format nil "~a ~a" (gethash :state h) c))
  (cond
    ((match-mul c) (handle-mul h c))
    ((equal c #\() (handle-mul h c))
    ((match-num c) (handle-num h c))
    ((equal c #\,) (handle-comma h c))
    ((equal c #\)) (handle-close h c))
    (t (clear-state h)))
  h)

(defun part1 (filename)
  (let* ((cs (uiop:read-file-string filename))
         (h (reduce
             (lambda (acc c)
               (handle-char acc c)
               acc)
             cs :initial-value
             (dict :state '() :param1 '() :param2 '() :prods '() :switch t))))
    (apply #'+ (gethash :prods h))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part2

(defun match-switch (c)
  (or (eq #\d c)
      (eq #\o c)
      (eq #\n c)
      (eq #\' c)
      (eq #\t c)
      (eq #\( c)))

(defun handle-mul2 (h c)
  (let* ((s (gethash :state h))
         (r (append s `(,c))))
    (cond
      ((and (gethash :switch h)
            (equal s '(#\m #\u #\l))
            (equal c #\())
       (setf (gethash :state h) '(*)))
      ((or (equal r '(#\m #\u #\l))
           (equal r '(#\m #\u))
           (equal r '(#\m)))
       (setf (gethash :state h) r))
      (t (clear-state h)))))

(defun handle-switch (h c)
  (let* ((s (gethash :state h))
         (r (append s `(,c))))
    (cond
      ((and (equal s '(#\m #\u #\l))
            (equal c #\())
       (setf (gethash :state h) '(*)))
      ((or (equal r '(#\m #\u #\l))
           (equal r '(#\m #\u))
           (equal r '(#\m)))
       (setf (gethash :state h) r))
      ((or (equal r '(#\d #\o #\n #\' #\t #\())
           (equal r '(#\d #\o #\n #\' #\t))
           (equal r '(#\d #\o #\n #\'))
           (equal r '(#\d #\o #\n)))
       (setf (gethash :state h) r))
      ((or (equal r '(#\d #\o #\())
           (equal r '(#\d #\o))
           (equal r '(#\d)))
       (setf (gethash :state h) r))
      (t (clear-state h)))))

(defun handle-char2 (h c)
  (print (format nil "~a ~a ~a" (gethash :state h) c (gethash :switch h)))
  (cond
    ((match-mul c) (handle-switch h c))
    ((match-switch c) (handle-switch h c))
    ((equal c #\() (handle-switch h c))
    ((match-num c) (handle-num h c))
    ((equal c #\,) (handle-comma h c))
    ((equal c #\)) (handle-close h c))
    (t (clear-state h)))
  h)

(defun part2 (filename)
  (let* ((cs (uiop:read-file-string filename))
         (h (reduce
             (lambda (acc c)
               (handle-char2 acc c)
               acc)
             cs :initial-value
             (dict :state '() :param1 '() :param2 '() :prods '() :switch t))))
    (values (apply #'+ (gethash :prods h)) h)
    ))

;; 81403201
