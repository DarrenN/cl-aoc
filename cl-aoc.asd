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
               "parseq"
               "cl-ppcre"
               "trivia"
               "uiop")
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "2024/day1")
                 (:file "2024/day2")
                 (:file "2024/day3")
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
