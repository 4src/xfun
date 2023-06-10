(defmacro ? (s x &rest xs)
   "macro for recursive slot-values"
   (if (null xs) 
     `(slot-value ,s ',x)
     `(? (slot-value ,s ',x) ,@xs)))

(defmacro settings (&key about copyright usage args)
  `(progn (defstruct settings 
            (_what ,about)
            (_how  ,copyright)
            (_usage ,usage)
            (_slots ',args)  
            ,@(loop :for (slot flag want __ val) :in args :collect (list slot val)))
          (make-instance 'settings)))

(defun cli (settings
             &aux it (args #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*))
  (loop :for (slot flag want _ val) :in (? settings _slots) :do
    (if (setf it (member flag args :test #'equal))
      (setf (slot-value settings slot)
            (if (equal want "")
              (not val)
              (let ((n (read-from-string (second it) nil nil))) 
                (if (numberp n) n (second it))))))))

(defun show-help (settings)
  (format t "~a~%~a~%~%USAGE:~%  ~a~%~%OPTIONS:~%~%" 
          (? settings _what) (? settings _how) (? settings _usage)) 
  (loop :for (_ flag want help val) :in (? settings _slots) :do
    (format t "  ~a  ~5a  ~25a = ~a ~%" flag want help val)))



