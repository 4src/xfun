(defpackage :ezr (:use :cl))
(in-package :ezr)

;;;; preamble ------------------------------------------------------------------
(defstruct about 
  "struct for file meta info"
  (what "a.lisp")
  (why  "fun")
  (when "(c) 2024")
  (how  "BSD-2 license")
  (who  "Tim Menzies"))

(defstruct bayes 
  "struct for bayes settings" 
  (m 1) (k 2))

(defstruct settings 
  "struct for all settings"
  (about (make-about))
  (bayes (make-bayes))
  (train "data/auto93.lisp")
  (seed  1234567891))

(defvar *settings* (make-settings))

;;; structs 
(defstruct (data (:constructor %make-data)) 
  "place to store rows, sumamrizes in cols"
  rows cols)

(defstruct (cols (:constructor %make-cols)) 
  "factory that makes and stores  columns from row1 names"
  names all x y)

(defstruct col 
  "super class of num and sym"
  (n 0) (at 0) (name " "))

(defstruct (num (:include col) (:constructor %make-num))
  "summarize numeric columns"
  (goal 1) (mu 0) (m2 0) (sd 0) (lo 1E32) (hi -1E32))

(defstruct (sym (:include col)) 
  "summarize symbolic columns"
  has mode (most 0))

;;; macros 
(defmacro o (x f &rest fs) 
  "access slots in x via a chain of slot accessor"
  (if fs `(o (slot-value ,x ',f) ,@fs) `(slot-value ,x ',f)))

(defmacro ? (&rest slots) 
  "access settings"
  `(o *settings* ,@slots))

(set-macro-character #\$ #'(lambda (s _) 
                             "turn $x ==> (slot-value self 'x)"
                             `(slot-value self ',(read s t nil t))))

(defmacro seen (lst x &optional (init 0))
  "increment or create a symbol count. not recommended for > 30 unique symbols"
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

;;;; create ---------------------------------------------------------------------
(defun make-num (&key (at 0)  (name " ") &aux (self (%make-num :at at :name name)))
  (setf $goal (if (eq (last-char name) #\-) 0 1))
  self)

(defun make-cols (names &aux (self (%make-cols :names names)))
  (dolist (name names self)
    (push (make-cols1 self name :a (char name 0) :z (last-char name))
          $all)))

(defun make-cols1 (self name &key a z)
  (let* ((what (if (upper-case-p a) #'make-num #'make-sym))
         (col  (funcall what :name name :at (length $all))))
    (unless (eql z #\X) 
      (if (member z (list #\! #\- #\+)) (push col $y) (push col $x)))
    col))

(defun make-data (&optional src &aux (self (%make-data)))
  (labels ((fun (row) (add self row)))
    (if (stringp src) (mapread #'fun src) (mapcar #'fun src))
    self))

;;;; add -----------------------------------------------------------------------
(defmethod add ((self data) (row cons))
  (if $cols
    (push (add $cols row) $rows)
    (setf $cols (make-cols row))))

(defmethod add ((self cols) row)
  (dolist (lst (list $x $y) row)
    (dolist (col lst)
      (add col (elt row (col-at col))))))

(defmethod add ((self col) x)
  (unless (eq x '?)
     (incf $n)
     (add1 self x)))

(defmethod add1 ((self num) (x number)) ; -> nil
  (let ((d (- x $mu)))
    (incf $mu (/ d $n))
    (incf $m2 (* d (-  x $mu)))
    (setf $lo (min x $lo)
          $hi (max x $hi)
          $sd (if (< $n 2) 0 (sqrt (/ $m2 (- $n 1)))))))

(defmethod add1 ((self sym) x) ; --> nil
  (let ((new (incf (seen $has x))))
    (if (> new $most)
      (setf $mode x
            $most new))))

;;;; misc -----------------------------------------------------------------------
(defmethod mid ((self num)) 
  "mid for NUMs is mu"   
  $mu)   

(defmethod mid ((self sym)) 
  "mid for SYMs is mode" 
  $mode) 

(defmethod div ((self num)) ; --> float
  "NUMbers have standard deviation"
  (if (< $n 2) 0 (sqrt (/ $m2 (- $n 1)))))

(defmethod div ((self sym)) ; --> float
  "SYMbols have entropy"
  (* -1 (loop :for (_ . v) :in $has :sum (* (/ v $n) (log (/ v $n) 2)))))

;;;; bayes -----------------------------------------------------------------------
(defmethod like ((self data) row &key nall nh) ; --> float
  "return likelihood of a row"
  (let* ((prior (/ (+ (length $rows) (? bayes k)) 
                   (+ nall (* nh (? bayes k))))))
    (+ (log prior) (loop :for col :in (o $cols x) :sum (_loglike row col prior)))))

(defun _loglike (row col prior &aux (x (cell col row))) ; --> float
  "usually, return log of likelihood (but for dontknow and zero, return 0)"
  (unless (eql x '?)
    (let ((tmp (like col x :prior prior)))
      (unless (zerop tmp) 
        (return-from _loglike (log tmp)))))
  0)

(defmethod like ((self sym) x &key prior) ; --> float
  "return likelhood of a SYMbol"
  (/ (+ (count $seen x) (* (? bayes m) prior)) 
     (+ $n (? bayes m))))

(defmethod like ((self num) x &key prior) ; --> float
    "return likelhood of a NUMber"
  (let ((sd (+ $sd 1E-30)))
    (/ (exp (- (/ (expt (- x $mu) 2) (* 2 (expt sd 2)))))
       (* sd (sqrt (* 2 pi))))))

;;;; utils -----------------------------------------------------------------------
(defun last-char (s) (char s (1- (length s)))) 

(defun mapread (fun file)
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

;;;; start-up -----------------------------------------------------------------------
;;; egs
(defun eg-one   (x) (format t "~a~%" (? bayes k)))
(defun eg-mapread (f) (mapread #'print (? train) ))
(defun eg-data  (&optional file) 
  (dolist (col (o (make-data (or file (? train))) cols y)) 
    (format t "~a~%" col)))

;;; main 
(defun main (args)
  (loop :for (flag arg) :on args :by #'cdr :do
    (let ((fun (intern (format nil "EG~:@(~a~)" flag))))
      (if (fboundp fun)
        (funcall fun (if arg (str2thing arg)))))))

(main #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*)
