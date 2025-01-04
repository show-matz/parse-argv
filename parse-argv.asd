
(asdf:defsystem #:parse-argv
  :author "Show Matsuoka <show.matsuoka@gmail.com>"
  :description "A library for parsing command line arguments."
  :licence "MIT"
  :version "0.1"
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "parse-argv")))

(asdf:defsystem #:parse-argv/tests
  :author "Show Matsuoka <show.matsuoka@gmail.com>"
  :description "Test of parse-argv."
  :licence "MIT"
  :version "0.1"
  :depends-on (:parse-argv :fiveam)
  :serial t
  :components ((:file "tests")))

(defmethod asdf:perform ((op test-op)
                         (c (eql (find-system :parse-argv))))
  (require :parse-argv/tests)
  (funcall (read-from-string "5AM:RUN!") :parse-argv))

