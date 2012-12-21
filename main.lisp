(in-package #:incognito-keywords)

(defvar *%package*
  (or (find-package '#:ikeyword)
      (make-package '#:ikeyword :use nil :nicknames '(#:i))))

(defvar *%cl-package* (find-package '#:cl))

(declaim (inline package))
(defun ikeywords:package ()
  *%package*)

(defun ikeywords:ikeywordp (object)
  (and (symbolp object)
       (eq (symbol-package object) (ikeywords:package))))

(defun ikeywords:ensure (name)
  (setf name (string name))
  (when (find-symbol name *%cl-package*)
    (error "Can't create an ikeyword named ~S ~
            since a symbol with that name already exists ~
            in the ~A package."
           name (package-name *%cl-package*)))
  (let* ((ikeyword-package (ikeywords:package))
         (symbol (intern name ikeyword-package)))
    (export symbol ikeyword-package)
    symbol))

(defmacro ikeywords:defpackage (name &rest options)
  (let* ((exported nil)
         (clauses
          (map-bind (mapcar) ((option options))
            (destructuring-bind (operator &rest arguments) option
              (ecase operator
                (:export
                 (push (copy-seq arguments) exported)
                 option)
                ((:nicknames :documentation :size)
                 option))))))
    `(progn
       (enhanced-eval-when:eval-when t
         (mapcar #'ikeywords:ensure
                 ',(reduce #'nconc (nreverse exported) :from-end t)))
       (cl:defpackage ,name
         (:use #:ikeyword)
         ,@clauses))))
