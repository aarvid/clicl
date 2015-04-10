(in-package :clicl-macros)

(cl:defmacro clicl-macros:defun (name lambda-list cl:&body body)
 
  
  `(progn
     (cl:defun ,name ,lambda-list ,@body)))


