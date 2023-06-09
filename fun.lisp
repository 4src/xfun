; vi: set ts=2 sw=2 sts=2 et :
(defvar *settings* '("
fun.lisp: LISP code for multi-objective semi-supervised explanations
(c) 2023, Tim Menzies"
  (bins "-b" "number of bins"      16)
  (file "-f" "where to read data"  "../data/auto93.lisp")
  (go   "-g" "start up action"     nothing)
  (help "-h" "show help"           nil)
  (seed "-s" "set random seed"     1234567891)
  ))

(defmacro ? (key)
  "Access setting value.s"
  `(fourth (assoc ',key (cdr *settings*))))

(defmacro freq (x lst &optional (init 0))      
  "frequency counts for small group of symbols (say, less than 50)"
  `(cdr (or (assoc ,x ,lst :test #'equal) 
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

(defstruct (data (:constructor %make-data)) rows cols)
(defstruct (cols (:constructor %make-cols)) names all x y klass)

(defstruct sym 
  "summarizes a stream of symbols"
  (at 0) (txt "") (n 0) has (most 0) mode)

(defstruct (num  (:constructor %make-num))
  "summarizes a stream of numbers"
  (at 0) (txt "") (n 0) (w 1) ; w=1,-1 means "maximize", "minimize"
  (hi most-negative-fixnum) 
  (lo most-positive-fixnum)
  (mu 0) (m2 0))

(defun make-num (&key (at 0) (txt "") &aux (self (%make-num :txt txt :at at)))
  (if (eql #\- (last-char txt))
    (setf (num-w self)  -1))
  self)

(defun make-cols (names &aux (self (%make-cols :names names)))
  (with-slots (all x y klass) self
    (loop :for at :from 0 :and s :in names :do
          (let* ((z    (last-char s))
                 (what (if (upper-case-p (char s 0)) #'make-num #'make-sym))
                 (col  (funcall what :at at :txt s)))
            (push col all) 
            (unless (eql z  #\X)
              (if (eql z #\!) (setf klass col))
              (if (member z '(#\+ #\- #\!)) (push col y) (push col x))))))
  self)

(defun make-data (&key cols  rows 
                  &aux (self (%make-data :cols (make-cols cols) :rows rows)))
  (dolist (row rows self)
    (mapc #'add (cols-all (data-cols self)) row)))

(defmethod add ((self sym) x)
  "update frequency counts (in `has`) and `most` and `mode`"
  (with-slots (has n mode most) self
   (unless (eq x '?)
     (incf n)
     (incf (freq x has))
     (if (> (freq x has) most) (setf most (freq x has) mode x)))))

(defmethod add ((self num) x ) ;;; Add one thing, updating 'lo,hi'
  "updates `lo`, `hi`, `mu`, `sd`"
  (with-slots (n lo hi mu m2) self
    (unless (eq x '?)
      (incf n)
      (let ((d (- x mu)))
        (incf mu (/ d n))
        (incf m2 (* d (- x mu)))
        (setf lo (min x lo)
              hi (max x hi))))))

; ---------------------------------------------------------------
(defmethod mid ((self sym) &optional places) (sym-mode self))
(defmethod mid ((self num) &optional places) (rnd (num-mu self) places))

(defmethod div ((self sym) &optional places)
  "diversity (entropy)."
  (with-slots (has n) self 
    (labels ((fun (p) (if (<= p 0) 0 (* -1 (* p (log p 2))))))
      (rnd (loop for (_ . n1) in has sum (fun (/ n1 n))) places))))

(defmethod div ((self num) &optional places)
  "return standard deviation"
  (with-slots (n m2) self 
    (rnd (if (<= n 1) 0 (sqrt (/ m2 (- n 1)))) places)))

(defmethod stats ((self data) &key (places 2) (fun #'mid) (cols (cols-y (data-cols self))))
  (mapcar #'(lambda (col) (cons (slot-value col 'txt) 
                                (funcall fun col places))) cols))

;;;; ---------------------------------------------------------------
;;; number stuff
(defun rnd (n &optional places)
  (if places
    (let ((div (expt 10 places)))
      (float (/ (round (* (float n) div)) div)))
    n))

;;; symbol stuff
(defun last-char(s)
  (if (> (length s) 0) (char s (1- (length s)))))

;;; file stuff
(defun file->data (file)
  "read data from file"
  (with-open-file (s file) 
    (let ((lst  (read s nil nil)))
      (make-data :cols (pop lst) :rows lst))))

;;; settings stuff
(defun print-settings ()
  (format t "~a~%~%OPTIONS:~%~%" (car *settings*))
  (loop :for (key flag help val) :in (cdr *settings*) :do
        (format t "  ~3a ~6a ~25a =  ~a ~%" flag 
                (cond ((eq val t)  "") ((eq val nil) "") (t key)) help val)))

;;; random number stuff
(defvar *seed* 10013)

(defun rand (&optional (n 1))
  "random float 0.. < n"
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 1) &aux (base 10000000000.0))
  "random int 0..n-1"
  (floor (* n (/ (rand base) base))))

;;; command-line stuff
(defun updates (settings)
  "Replace setting values, if a command-line flag asks you."
  (dolist (four (cdr settings) settings)
    (let* ((it (member (second four) (args) :test #'equal)))
      (if it
        (setf (fourth four)  (update (fourth four) (second it)))))))

(defun update (current command-line-arg)
  "For booleans, no need of a command-line-arg,  just flip the value.
  Else try to read a number, and if that flag, just return as a string"
  (cond ((eql current  t)   nil)
        ((eql current  nil) t)
        (t (let ((n (read-from-string command-line-arg nil nil))) 
             (if (numberp n) n command-line-arg)))))

;;; system specific stuff
(defun args () 
  "Accessing command-line flags"
  #+clisp ext:*args*  
  #+sbcl sb-ext:*posix-argv*)

(defun bye (status)
  "Exit, returning status."
  #+clisp (ext:exit stutus)
  #+sbcl  (sb-ext:exit :code status))

;;;; unit test  stuff  ----------------------------------------------------------------
(defun main (tests)
  (let ((fails 0)
        (b4    (copy-tree (setf *settings* (updates *settings*)))))
    (if  (? help) 
      (print-settings)
      (loop :for (key fun) :in tests :do
        (setf *settings* (copy-tree b4)
              *seed* (? seed))
        (when (member (? go) (list "all" key) 
                      :key #'string-downcase :test #'equalp)
          (format t "~&~%⚠️  ~a " key) 
          (cond ((funcall fun) (format t " PASSED ✅~%"))
                (t             (format t " FAILED ❌~%")
                               (incf fails))))))
    fails))

(bye 
  (main 
    `(
      (settings ,(lambda () (print *settings*)))
      (rnd      ,(lambda () (print 1111) (print (rnd 3.14156 2)))) 
      (rand1    ,(lambda () (princ (rint 100)) (princ (rint 100))))
      (rand2    ,(lambda () (princ (rint 100))))
      (num1      ,(lambda (&aux (n (make-num)))
                   (dotimes (i 1000) (add n i))
                     (print (mid n))))
      (num2      ,(lambda (&aux (n (make-num)))
                   (dotimes (i 1000) (add n i))
                   (<= 288 (div n) 289)
                   (<= 499 (mid n) 501)  ))
      (sym      ,(lambda (&aux (s (make-sym)))
                   (dolist (x '(a a a a b b c)) (add s x))
                   (eql #\a (mid s))
                   (<= 1.37 (div s) 1.38)  ))
      (data     ,(lambda (&aux (d (file->data (? file))))
                   (print (cols-y (data-cols d)))
                   (eql 398 (length (data-rows d)))
                   (eql 4 (length (cols-x (data-cols d))))   ))
      (stats    ,(lambda (&aux (d (file->data (? file))))
                   (print (stats d))   ))
      )))
