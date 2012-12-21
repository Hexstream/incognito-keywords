(asdf:defsystem #:incognito-keywords

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "incognito-keywords introduces a new kind of keyword that looks just like any non-keyword symbol and allows safe usage of convenient but clashy symbol names by multiple libraries without conflicts through sharing. Some names that might benefit are (alist blist plist macro operator index &doc &decl &rest+ &destructure &ignored &ignorable)."

  :depends-on (#:map-bind
               #:enhanced-eval-when)

  :version "1.1"
  :serial cl:t
  :components ((:file "package")
	       (:file "main")))
