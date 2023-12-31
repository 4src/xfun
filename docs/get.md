# get.lisp

#+clisp
(defun loader (x) (load x))
#+sbcl
(defun loader (x)
  (handler-bind
      ((simple-warning 
        #'(lambda (w) 
            (when (undefined-variable-warning-p w)
              (invoke-restart 'muffle-warning)))))
      (load x))
    (load x))
