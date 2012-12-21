(cl:defpackage #:incognito-keywords
  (:nicknames #:ikeywords)
  (:use #:cl)
  (:import-from #:map-bind #:map-bind)
  (:shadow #:package
           #:defpackage)
  (:export #:defpackage
           #:ensure
           #:package
           #:ikeywordp))
