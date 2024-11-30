(defsystem "cl-aoc"
  :version "0.1.0"
  :author "DarrenN"
  :license "MIT"
  :depends-on ("access"
               "alexandria"
               "arrow-macros"
               "fset"
               "serapeum"
               "str"
               "parse-float"
               "parse-number"
               "cl-ppcre"
               "uiop")
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "day1"))))
  :description "Advent of Code in Common Lisp (SBCL)"
  :in-order-to ((test-op (test-op "cl-aoc/tests"))))

(defsystem "cl-aoc/tests"
  :author "DarrenN"
  :license "MIT"
  :depends-on ("cl-aoc"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-aoc"
  :perform (test-op (op c) (symbol-call :rove :run c)))
