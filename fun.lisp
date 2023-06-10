; vim: set ts=2 sw=2 sts=2 et :
(defvar *settings* '("
fun.lisp: LISP code for multi-objective semi-supervised explanations
(c) 2023, Tim Menzies"
  (bins "-b" "number of bins"      16)
  (file "-f" "where to read data"  "../data/auto93.lisp")
  (go   "-g" "start up action"     nothing)
  (help "-h" "show help"           nil)
  (seed "-s" "set random seed"     1234567891)
  ))

(defun goodbye (&optional (status 0))
 "Exit, returning status."
 #+clisp (ext:exit status)
 #+sbcl  (sb-ext:exit :code status))

(defmacro ? (key)
  "Access setting value.s"
  `(fourth (assoc ',key (cdr *settings*))))

(defmacro freq (x lst &optional (init 0))      
  "frequency counts for small group of symbols (say, less than 50)"
  `(cdr (or (assoc ,x ,lst :test #'equal) 
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

(defmacro of (s x &rest xs)
  "macro for recursive slot-values"
  (if (null xs) 
    `(slot-value ,s ',x)
    `(of (slot-value ,s ',x) ,@xs)))


;;;; ---------------------------------------------------------------
(defstruct (data (:constructor %make-data)) rows cols)
(defstruct (cols (:constructor %make-cols)) names all x y klass)
(defstruct sym (at 0) (txt "") (n 0) has (most 0) mode)

(defstruct (num  (:constructor %make-num))
 "summarizes a stream of numbers"
  (at 0) (txt "") (n 0) (w 1) ; w=1,-1 means "maximize", "minimize"
  (hi most-negative-fixnum) 
  (lo most-positive-fixnum)
  (mu 0) (m2 0))
;;;; ---------------------------------------------------------------
(defun make-num (&key (at 0) (txt ""))
  (%make-num :txt txt :at at :w (if (eql #\- (last-char txt)) -1 1)))

(defun make-cols (names &aux (self (%make-cols :names names)) (at -1))
  "[string]+ -> [col]+"
  (with-slots (all x y klass) self
    (labels 
      ((make-col (name)
         (let* ((a     (char name 0))
                (z     (last-char name))
                (maker (if (upper-case-p a) #'make-num #'make-sym))
                (col   (funcall maker :at (incf at) :txt name)))
           (unless (eql z  #\X)
             (if (eql z #\!) (setf klass col))
             (if (member z '(#\+ #\- #\!)) (push col y) (push col x)))
           col)))
      (setf all (mapcar #'make-col names))))
  self)

(defun make-data (&key cols rows &aux (self (%make-data :cols (make-cols cols))))
  "([string]+, [list]) -> data"
  (dolist (row rows self) (add self row)))

(defun clone (data &optional rows)
  "data -> data"
  (make-data :cols (of data cols names) :rows rows))

;;;;; ---------------------------------------------------------------
(defmethod add ((d data) row)
  "Add a new row, summarizing its contents as we go."
  (push (mapc #'add (of d cols all) row) (of d rows)))

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

(defun norm (num x)
  (/ (- x (of num lo))
     (- (of num hi) (of num lo) (/ 1 most-positive-fixnum))))

(defun stats (data &key (places 2) (fun #'mid) (cols (of data cols y)))
  (mapcar #'(lambda (col) (cons (slot-value col 'txt) 
                                (funcall fun col places))) cols))

(defun better (data row1 row2)
  (let* ((s1   0) 
         (s2   0) 
         (cols (of data cols y))
         (n    (length cols)))
    (dolist (col cols (< (/ s1 n) (/ s2 n)))
      (with-slots (at w) col
        (let ((x (norm col (elt row1 at)))
              (y (norm col (elt row2 at))))
          (decf s1 (exp (* w (/ (- x y) n))))
          (decf s2 (exp (* w (/ (- y x) n)))))))))
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

;; file stuff
(defun file->data (file &aux (lst (reads file)))
  (make-data :cols (pop lst) :rows lst))

;; file stuff
(defun reads (file)
  "read data from file"
  (with-open-file (s file) (read s nil nil)))

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

;;;;; unit test  stuff ----------------------------------------------------------------
(defun main (tests)
 (setf *settings* (updates *settings*))
 (let ((fails 0)
       (b4    (copy-tree *settings*)))
   (if  (? help) 
     (print-settings)
     (loop :for (key fun) :in tests :do
       (setf *settings* (copy-tree b4)
             *seed*     (? seed))
       (when (member (? go) (list "all" key) :key #'string-downcase :test #'equalp)
         (format t "~&~%⚠️  ~a " key) 
         (cond ((funcall fun) (format t " PASSED ✅~%"))
               (t             (format t " FAILED ❌~%")
                              (incf fails))))))
   fails))

(goodbye (main 
   `(
     ;(bad      ,(lambda () nil))
     (settings ,(lambda () (print *settings*)))
     (rnd      ,(lambda () (print 1111) (print (rnd 3.14156 2)))) 
     (rand1    ,(lambda () (princ (rint 100)) (princ (rint 100))))
     (rand2    ,(lambda () (princ (rint 100))))
     (num1     ,(lambda (&aux (n (make-num)))
                  (dotimes (i 1000) (add n i))
                    (print (mid n))))
     (num2     ,(lambda (&aux (n (make-num)))
                  (dotimes (i 1000) (add n i))
                  (<= 288 (div n) 289)
                  (<= 499 (mid n) 501)  ))
     (sym      ,(lambda (&aux (s (make-sym)))
                  (dolist (x '(a a a a b b c)) (add s x))
                  (eql #\a (mid s))
                  (<= 1.37 (div s) 1.38)  ))
      (data    ,(lambda (&aux (d (file->data (? file))))
                  (print (of d cols y))
                  (eql 398 (length (of d rows)))
                  (eql 4 (length (of d cols x)))   ))
     (stats    ,(lambda (&aux (d (file->data (? file))))
                  (print (stats d))   ))
     (better   ,(lambda (&aux (d (file->data (? file))))
                  (sort (of d rows) (lambda(r1 r2) (better d r1 r2)))))
                 
     )))
