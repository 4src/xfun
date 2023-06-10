(defmacro settings (doc &rest slots)
  `(defstruct settings ,doc (_slots ',slots)  
      ,@(loop :for (slot flag help val) :in slots :collect `(,slot ,val))))

(settings 
"asdas asdas sadas 
asdasdassa"
  (bins "-b" "how many bins"      16)
  (file "-f" "where to dead data" "../data/auto93.lisp")
  (help "-h" "show help"          nil)
  (min  "-m" "min data size"      .5)
  (seed "-s" "random bumber seed" 1234567891))

(defun cli (settings)
  (loop :for (slot flag help val) :in (slot-value settings '_slots) :do
    (aif (member flag (args) :test #'equal)
      (setf (slot-value settings slot) (cond ((eql val  t)   nil)
                                             ((eql val  nil) t)
                                             (t (let ((n (read-from-string (second it) nil nil))) 
                                                  (if (numberp n) n (second it))))))))
  (when (slot-value settings 'help) 
    (format t "~a~%~%OPTIONS:~%~%" (documentation 'settings 'structure))
    (loop :for (slot flag help val) :in (slot-value settings  '_slots) :do
      (format t "  ~3a ~6a ~25a =  ~a ~%" flag 
              (cond ((eq val t)  "") ((eq val nil) "") (t slot)) help val)))
  settings)

(defmacro ? (s x &rest xs)
   "macro for recursive slot-values"
   (if (null xs) 
     `(slot-value ,s ',x)
     `(? (slot-value ,s ',x) ,@xs)))

(defvar my (make-settings))

(print (? my seed))
