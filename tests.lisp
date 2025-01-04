(in-package :parse-argv)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :5am))

(5am:in-suite* :parse-argv)

(defmacro check-equal ((&rest expected) (&rest input) (&rest option-rules) (&rest param-rules))
  `(5am:is (equal ',expected (argv:parse ',input ',option-rules ',param-rules))))

(defun check-err-impl (expected-error input option-rules param-rules)
  (handler-case
	  (prog1 nil
		(argv:parse input option-rules param-rules))
	(error (e)
	  (typep e expected-error))))

(defmacro check-error (expected-error (&rest input) (&rest option-rules) (&rest param-rules))
  `(5am:is (check-err-impl ',expected-error ',input ',option-rules ',param-rules)))

(5am:test parse-argv-1

  ;; no options, no params.
  (check-equal () ()     nil nil)
  (check-equal () ("--") nil nil)

  ;; parameters.
  (check-equal (123)        ("123")    nil (:integer))
  (check-error error        ("123x")   nil (:integer))    ; error: invalid integer value.
  (check-equal (:FOO)       ("foo")    nil (:keyword))
  (check-equal ("string")   ("string") nil (:string))
  (check-equal (T)          ("true")   nil (:boolean))
  (check-equal (NIL)        ("false")  nil (:boolean))
  (check-error simple-error ("none")   nil (:boolean))    ; error: invalid boolean value.
  (check-error simple-error ("oops")   nil (:unknown))    ; error: invalid type.
  (check-equal (123   :FOO  "string" T)
			   ("123" "foo" "string" "true")
			   nil
			   (:integer :keyword :string :boolean))

  ;; optional parameters.
  (check-equal (T      "string"  123 :BAR)
			   ("true" "string" "123")
			   nil
			   (:boolean :string &optional (:integer 0) (:keyword :bar)))
  (check-error simple-error
			   ("string" "123")
			   nil
			   (:string &optional (:integer 0) &optional (:keyword :bar))) ; error: duplicated &optional.
  ;; rest parameters.
  (check-equal (T      "string" ("123" "true" "bar"))
			   ("true" "string"  "123" "true" "bar")
			   nil
			   (:boolean :string &rest))

  ;; parameter length.
  (check-error simple-error
			   ("true")
			   nil
			   (:boolean :string &optional (:integer 0))) ; error: too less parameters.
  (check-error simple-error
			   ("true" "string" "123" "extra")
			   nil
			   (:boolean :string &optional (:integer 0))) ; error: too more parameters.

  ;; options.
  (check-equal (:EXTRA T) ("-x")          ((:extra #\x))                      nil)
  (check-equal (:ALL   T) ("-a")          ((:all   #\a "--all"))              nil)
  (check-equal (:ALL   T) ("--all")       ((:all   #\a "--all"))              nil)
  (check-equal (:COUNT 5) ("-c" "5")      ((:count #\c "--count" :integer 3)) nil)
  (check-equal (:COUNT 5) ("-c5")         ((:count #\c "--count" :integer 3)) nil)
  (check-equal (:COUNT 5) ("--count" "5") ((:count #\c "--count" :integer 3)) nil)
  (check-equal (:COUNT 5) ("--count=5")   ((:count #\c "--count" :integer 3)) nil)
  (check-equal (:COUNT 3) ("-c")          ((:count #\c "--count" :integer 3)) nil)
  (check-equal (:COUNT 3) ("--count")     ((:count #\c "--count" :integer 3)) nil)

  (check-error simple-error ("-y")  ((:extra #\x))                    nil) ; error: invalid option.
  (check-error simple-error ("-X")  ((:extra #\x))                    nil) ; error: invalid option.
  (check-error simple-error ("--x") ((:extra #\x "--extra"))          nil) ; error: invalid option.
  (check-error simple-error ("-c")  ((:count #\c "--count" :integer)) nil) ; error: default value missing.
  (check-error simple-error ("--x=value") ((:extra #\x "--x")) nil) ; error: invalid option '--x=value'.

  ;; chunk of short options.
  (check-equal (:X T :Y T :Z T) ("-xyz")   ((:x #\x) (:y #\y) (:z #\z))     nil)
  (check-equal (:X T :Y 100)    ("-xy100") ((:x #\x) (:y #\y nil :integer)) nil)

  ;; chomp of long option.
  (check-equal (:COUNT 8) ("--cou=8")  ((:count #\c "--count" :integer 3)) nil)
  (check-error simple-error ("--coun") ((:count   #\c "--count")
                                        (:council #\C "--council")) nil)    ; error: ambiguous option.

  ;; end of options.
  (check-equal (5 :COUNT 3)
			   ("-c" "--" "5")
			   ((:count #\c "--count" :integer 3))
			   (:integer))

  ;; all stuff.
  (check-equal (123 "string" :BAR ("0" "x") :ALL T :MODE :EXTRA :COUNT 3)
			   ("--all" "--mode=extra" "-c" "--" "123" "string" "bar" "0" "x")
			   ((:all   #\a "--all")
				(:mode  #\m "--mode"  :keyword :NORMAL)
				(:count #\c "--count" :integer 3))
			   (:integer :string &optional (:keyword :foo) &rest)))
