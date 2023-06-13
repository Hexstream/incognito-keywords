(cl:defpackage #:incognito-keywords
  (:nicknames #:ikeywords)
  (:use #:cl)
  (:shadow #:package
           #:defpackage)
  (:export #:defpackage
           #:ensure
           #:package
           #:ikeywordp))
