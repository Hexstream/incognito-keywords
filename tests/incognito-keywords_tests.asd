(asdf:defsystem #:incognito-keywords_tests

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "incognito-keywords unit tests."

  :depends-on ("incognito-keywords"
               "parachute")

  :serial cl:t
  :components ((:file "tests"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:incognito-keywords_tests)))
