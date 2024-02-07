; vim : sw=2 ts=2 et :

(defmacro ? (x) `(third (slot-value *the* ',x)))

(defmacro o (struct slot &rest slots) 
  (if slots `(o (slot-value ,struct ',slot) ,@slots)  `(slot-value ,struct ',slot)))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
       (if it ,then ,else)))

(defmacro stuff (&rest things)
  (labels ((name  (s) (intern (format nil "~:@(~a~)" s)))
           (maker (s) (intern (format nil "%MAKE-~a" (name s)))))
    `(progn 
       ,@(loop for (defstruct isa . slots) in things collect 
           `(progn (defstruct (,(name isa) (:constructor ,(maker isa))) ,@slots)
                   (defmethod slots-of ((_ ,(name isa))) 
                     ',(mapcar (lambda (x) (if (listp x) (car x) x)) slots)))))))

(defun safe-read (s)
  (let ((*read-eval* nil)) (read-from-string s)))

(defun slurp (file)
  (with-open-file (s txt) (safe-read s)))

(defun cli (struct &optional (slots (slots-of struct))
                   (args #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*))
  (dolist (slot slots struct)
    (destructuring-bind (flag _ b4) (slot-value struct slot)
      (aif (member flag args :test #'string=)
        (setf (third (slot-value struct slot))
              (cond ((eq b4 t) nil)
                    ((eq b4 nil) t)
                    (t (safe-read (second it)))))))))

(stuff 
  (defstruct my
    (file '("-f" "file" "../data/auto93.lisp"))))

(defvar *the* (cli (%make-my)))

(defun make-col (s at)s  
  (let* ((a (char s 0))
         (z (char s (1- (length s))))
         (w (if (eql z #\-) 0 1)))
    (if (upper-case-p a) 
      (make-num :txt s :at at :w w) 
      (make-sym :txt s :at at :w w))))

(defun cols (names)
  (let  (all xs ys (at 0)) 
    (dolist (s txts)
      (let* ((a (char s 0))
             (z (char s (1- (length s))))
             (w (if (eql z #\-) 0 1))
             (col (if (upper-case-p a) 
                    (make-num :txt s :at (incf at) :w w) 
                    (make-sym :txt s :at (incf at) :w w))))
        (push col all)
        (unless (eql #\X z)
          (if (member z (list #\- #\+ #\!))
            (push col ys)
            (push col xs)))))
    (make-data :all all :xs xs :ys ys)))

    
(print (slurp (o my file)))

