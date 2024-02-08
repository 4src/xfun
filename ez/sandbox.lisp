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

(defun safe-slurp (file)
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
    (file '("-f" "file" "../data/auto93.lisp")))

  (defstruct data rows cols all xs ys)

  (defstruct sym
    (at 0) (txt "") (n 0) goalp has)

  (defstruct num
    (at 0) (txt "") (n 0) (mu 0) (m2 0) (sd 0) goalp
    (lo most-positive-fixnum)
    (hi most-negative-fixnum)))

(defvar *the* (cli (%make-my)))

(defun make-num (txt at &aux (z (char txt (1- (length txt)))))
  (%make-num :txt txt :at at :w (if (eql z #\-) 0 1)))
  
(defun make-col (s at)  
  (funcall (if (upper-case-p (char s 0)) #'make-num #'%make-sym) :txt s :at at)) 

(defun make-cols (data1 row &aux (at -1))
  (dolist (s row)
    (let* ((z   (char s (1- (length s))))
           (col (make-col s (incf at))))
      (push col (o data1 cols))
      (unless (eql z #\X)
        (setf ( ocol goalp) (member z (list #\- #\+ #\!)))))))

(defun make-data (&optional rows order &aux (data1 (%make-data))
  (dolist (row rows) (add data1 row))
  (if order (sort (o data1 rows) #'< :key (lambda (row) (d2h data1 row))))
  data1)

(defmethod add ((data1 data) row)
  (with-slots (rows cols) data
    (if cols
      (push (mapc #'add cols row) rows)
      (make-cols data1 row))))
    
(print (slurp (o my file)))

