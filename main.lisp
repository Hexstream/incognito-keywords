(in-package #:incognito-keywords)

(defvar *%package*
  (or (find-package '#:ikeyword)
      (make-package '#:ikeyword :use nil)))

(defvar *%cl-package* (find-package '#:cl))

(declaim (inline package))
(defun package ()
  *%package*)

(defun ensure (name)
  (setf name (string name))
  (when (find-symbol name *%cl-package*)
    (error "Can't create an ikeyword named ~S ~
            since a symbol with that name already exists ~
            in the ~A package."
           name (package-name *%cl-package*)))
  (let* ((ikeyword-package (package))
         (symbol (intern name ikeyword-package)))
    (export symbol ikeyword-package)
    symbol))

(defmacro defpackage (name &rest options)
  (let* ((docstring nil)
         (exported
          (map-bind (mapcan) ((option options))
            (destructuring-bind (operator &rest arguments) option
              (ecase operator
                (:export (copy-seq arguments))
                (:documentation
                 (prog1 nil
                   (destructuring-bind (docstring-option) arguments
                     (check-type docstring-option string)
                     (if docstring
                         (error "Multiple :documentation options in ~S."
                                options)
                         (setf docstring docstring-option))))))))))
    `(progn
       (mapcar #'ensure ',exported)
       (cl:defpackage ,name
         (:use #:ikeyword)
         (:export ,@exported)))))
