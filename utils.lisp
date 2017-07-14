(in-package :dt)

(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
