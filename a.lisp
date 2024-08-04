;;;; config
; (defstruct about 
;   (what "a.lisp")
;   (why  "fun")
;   (when "(c) 2024")
;   (how  "BSD-2 license")
;   (who  "Tim Menzies"))

(defstruct bayes (m 1) (k 2))

(defstruct settings 
  ;(about (make-about))
  (bayes (make-bayes))
  (train "data/auto93.lisp")
  (seed  1234567891))

(defvar *settings* (make-settings))

;;;; structs
(defstruct (data (:constructor %make-data)) rows cols)
(defstruct (cols (:constructor %make-cols)) names all x y)

(defstruct col (n 0) (at 0) (name ""))
(defstruct (num (:include col) (:constructor %make-num))
                 (goal 1) (mu 0) (m2 0) (sd 0) (lo 1E32) (hi -1E32))
(defstruct (sym (:include col)) has mode (most 0))

;;; macros
(defmacro o (x f &rest fs) (if fs `(o (slot-value ,x ',f) ,@fs) `(slot-value ,x ',f)))

(defmacro ? (&rest slots) `(o *settings* ,@slots))

(set-macro-character #\$  #'(lambda (s _) `(slot-value self ',(read s t nil t))))

(defmacro seen (lst x &optional (init 0))
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

;;;
(defun make-num (&key (at 0)  (name " ") &aux (self (%make-num :at at :name name)))
  (let ((z (char name (1- (length name)))))
    (setf $goal (if (eq z #\-) -1 0))))

(defun make-cols (names &aux (self (%make-cols :names names)))
  (dolist (name names self)
    (push (make-cols1 self name (char name 0) (char name (1- (length name)))) 
          $all)))

(defun make-cols1 (self name a z)
  (let* ((what (if (upper-case-p a) #'make-num #'make-sym))
         (col  (funcall what :name name :at (length $all))))
    (print name)
    (unless (eql z #\X) 
      (if (member z (list #\! #\- #\+)) (push col $y) (push col $x)))
    col))

(defun make-data (src &aux (self (%make-data)))
  (labels ((fun (row) (print 10) (add self row)))
    (if (stringp src) (slurp #'fun src) (mapcar #'fun src))
    self))

(defmethod add ((self data) (row cons))
  (if $cols
    (push (add $cols row) $rows)
    (setf $cols (make-cols row)))
  (print 500))

(defmethod add ((self cols) row)
  (dolist (lst (list $x $y) row)
    (dolist (col lst)
      (print (list '--> 666 col))
      (add col (elt row (col-at col)))
      (print (list '<-- (col-at col) 400)))
    (print 999)))

(defmethod add ((self col) x)
  (unless (eq x '?)
     (incf $n)
     (add1 self x)))

(defmethod add1 ((self num) (x number)) ; -> nil
  (print 'aaa)
  (let ((d (- x $mu)))
    (incf $mu (/ d $n))
    (incf $m2 (* d (-  x $mu)))
    (setf $lo (min x $lo)
          $hi (max x $hi)))
          (print 'bbb))

(defmethod add1 ((self sym) x) ; --> nil
  (print 100)
  (let ((new (incf (seen $has x))))
    (if (> new $most)
      (setf $mode x
            $most new)))
  (print 300))

;;;
(defun slurp (fun file)
  (with-open-file (s file) 
    (loop (funcall fun (or (read s nil nil) (return))))))

(defun str2thing (s &aux (s1 (string-trim '(#\Space #\Tab) s))) 
  (let ((it (let ((*read-eval* nil)) (read-from-string s1 ""))))
    (cond ((numberp it)     it)
          ((eq it t)        t)
          ((eq it nil)      nil)
          ((string= it "?") '?)
          (t                s1))))

(defvar *seed* (? seed))

(defun rint (&optional (n 100) &aux (base 1E10)) 
  (floor (* n (/ (rand base) base))))

(defun rand (&optional (n 1)) 
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

;;;;
(defun eg-one   (x) (print (? bayes k)))
(defun eg-slurp (f) (slurp #'print f))
(defun eg-data  (_) 
  (dolist (col (o (make-data (? train)) cols y)) 
    (print col)))

;;;;
(defun main (args)
  (loop :for (flag arg) :on args :by #'cdr :do
    (let ((fun (intern (format nil "EG~:@(~a~)" flag))))
      (if (fboundp fun)
        (funcall fun (if arg (str2thing arg)))))))

(main #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*)
