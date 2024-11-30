(defpackage cl-aoc/tests/main
  (:use :cl
        :cl-aoc
        :rove))
(in-package :cl-aoc/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-aoc)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
