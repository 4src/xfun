(print 
  (with-open-file (s "../data/auto93.lisp") 
    (read s)))

(defmacro o (struct slot &rest slots) 
  (if slots
    `(o (slot-value ,struct ',slot) ,@slots)  
    `(slot-value ,struct ',slot)))

(defmacro my (&rest defstructs)
  (labels ((name  (s)   (intern (format nil "~:@(~a~)" s)))
           (maker (s)   (intern (format nil "%MAKE-~:@(~a~)" s)))
           (names (lst) (mapcar (lambda (x) (if (listp x) (car x) x)) lst)))
    `(progn 
       ,@(loop for (defstruct isa . slots) in defstructs collect 
           `(progn
              (defstruct (,(name isa) (:constructor ,(maker isa))) ,@slots)
              (defmethod slots-of ((_ ,(name isa))) ',(names slots)))))))

(my 
  (defstruct fred (a 1) (b 2))
  (defstruct jane (a 1) (b 2)))

(print (macroexpand-1 
 '(my 
  (defstruct fred (a 1) (b 2))
  (defstruct jane (a 1) (b 2)))))




