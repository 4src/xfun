; <!-- vim: set ts=2 sw=2 sts=2 et: -->
; ##  Preamble 
; ### Package
(defpackage :ezr (:use :cl))
(in-package :ezr)

; ### Settings
(defstruct about 
  "Struct for file meta info."
  (what  "mink.lisp")
  (why   "optimization via recursive kmeans",
  (when  "(c) 2024")
  (how   "BSD-2 license")
  (who   "Tim Menzies")
  (where "timm@ieee.org"))

(defstruct settings 
  "Struct for all settings."
  (seed  1234567891)
  (buckets 2)
  (p 2)
  (train "data/auto93.lisp"))

(defvar *settings* (make-settings))

; ###  Structs 
(defstruct (data (:constructor %make-data)) 
  "Place to store `rows`, sumamrizes in `cols`."
  rows cols)

(defstruct (cols (:constructor %make-cols)) 
  "Factory that makes and stores columns from row1 names."
  names all x y)

(defstruct col 
  "Super class of NUM and SYM."
  (n 0) (at 0) (name " "))

(defstruct (num (:include col) (:constructor %make-num))
  "Summarize NUMeric columns."
  (goal 1) (mu 0) (m2 0) (sd 0) (lo 1E32) (hi -1E32))

(defstruct (sym (:include col)) 
  "Summarize SYMbolic columns."
  has mode (most 0))

; ### Macros 
(defmacro o (x f &rest fs) 
  "Access slots in `x` via a chain of slot accessor."
  (if fs `(o (slot-value ,x ',f) ,@fs) `(slot-value ,x ',f)))

(defmacro ? (&rest slots) 
  "Access settings."
  `(o *settings* ,@slots))

(set-macro-character #\$ #'(lambda (s _) 
                             "Turn $x ==> (slot-value self 'x)."
                             `(slot-value self ',(read s t nil t))))

(defmacro seen (lst x &optional (init 0))
  "Increment or create a symbol count. Not recommended for > 30 unique symbols."
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

; ## Methods 
; ### Create 
(defun make-num (&key (at 0)  (name " ") &aux (self (%make-num :at at :name name)))
  "Set goal to 0,1 for goals to minimze, maximze."
  (setf $goal (if (eq (last-char name) #\-) 0 1))
  self)

(defun make-cols (names &aux (self (%make-cols :names names)))
  "Create one column per name, store them in `all`."
  (dolist (name names self)
    (let* ((a    (char name 0)) 
           (z    (last-char name))
           (what (if (upper-case-p a) #'make-num #'make-sym))
           (col  (funcall what :name name :at (length $all))))
      (push col $all)
      (unless (eql z #\X) 
        (if (member z (list #\! #\- #\+)) (push col $y) (push col $x)))))

(defun make-data (&optional src &aux (self (%make-data)))
  "Load in data from either a file or a list."
  (labels ((fun (row) (add self row)))
    (if (stringp src) (mapread #'fun src) (mapcar #'fun src))
    self))

; ###  Add 
(defmethod add ((self data) (row cons))
  "First time, create columns. Next, summarize `row` in `cols` and store in `rows`."
  (if $cols
    (push (add $cols row) $rows)
    (setf $cols (make-cols row))))

(defmethod add ((self cols) row)
  "Summarise a row in the `x` and `y` columns."
  (dolist (lst (list $x $y) row)
    (dolist (col lst)
      (add col (elt row (col-at col))))))

(defmethod add ((self col) x)
  "Summarize `x` in a column (unless it is don't know."
  (unless (eq x '?)
     (incf $n)
     (add1 self x)))

(defmethod add1 ((self num) (x number)) 
  "Update a NUM."
  (let ((d (- x $mu)))
    (incf $mu (/ d $n))
    (incf $m2 (* d (-  x $mu)))
    (setf $lo (min x $lo)
          $hi (max x $hi)
          $sd (if (< $n 2) 0 (sqrt (/ $m2 (- $n 1)))))))

(defmethod add1 ((self sym) x) 
  "Update a SYM."
  (let ((new (incf (seen $has x))))
    (if (> new $most)
      (setf $mode x
            $most new))))

; ### Misc
(defmethod mid ((self num)) 
  "The mid() for NUMs is mu."   
  $mu)   

(defmethod mid ((self sym)) 
  "Thhe mid() for SYMs is mode." 
  $mode) 

(defmethod div ((self num)) ; --> float
  "NUMbers have standard deviation"
  (if (< $n 2) 0 (sqrt (/ $m2 (- $n 1)))))

(defmethod div ((self sym)) ; --> float
  "SYMbols have entropy"
  (* -1 (loop :for (_ . v) :in $has :sum (* (/ v $n) (log (/ v $n) 2)))))

; ###  Bayes 
(defmethod like ((self data) row &key nall nh) ; --> float
  "Return the log likelihood of a row."
  (let* ((prior (/ (+ (length $rows) (? bayes k)) 
                   (+ nall (* nh (? bayes k))))))
    (+ (log prior) (loop :for col :in (o $cols x) :sum (_loglike row col prior)))))

(defun _loglike (row col prior &aux (x (cell col row))) ; --> float
  "Usually, return log of likelihood (but for dontknow and zero, return 0)."
  (unless (eql x '?)
    (let ((tmp (like col x :prior prior)))
      (unless (zerop tmp) 
        (return-from _loglike (log tmp)))))
  0)

(defmethod like ((self sym) x &key prior) ; --> float
  "Return likelhood of a SYMbol."
  (/ (+ (count $seen x) (* (? bayes m) prior)) 
     (+ $n (? bayes m))))

(defmethod like ((self num) x &key prior) ; --> float
  "Return likelhood of a NUMber."
  (let ((sd (+ $sd 1E-30)))
    (/ (exp (- (/ (expt (- x $mu) 2) (* 2 (expt sd 2)))))
       (* sd (sqrt (* 2 pi))))))

; ##  Utils 
; ### Strings
(defun last-char (s) 
  "Return last character in a string."
  (char s (1- (length s)))) 

(defun str2thing (s &aux (s1 (string-trim '(#\Space #\Tab) s))) 
  "Coerce `s` to an atomic thing."
  (let ((it (let ((*read-eval* nil)) (read-from-string s1 ""))))
    (cond ((numberp it)     it)
          ((eq it t)        t)
          ((eq it nil)      nil)
          ((string= it "?") '?)
          (t                s1))))

; ### Files
(defun mapread (fun file)
  "Run `fun` for all things in a file."
  (with-open-file (s file) 
    (loop (funcall fun (or (read s nil nil) (return))))))

; ### Random numbers
; Enables platform-independent seeding for random nums in LISP."
(defvar *seed* (? seed))

(defun rint (&optional (n 100) &aux (base 1E10)) 
  "Return a random integer."
  (floor (* n (/ (rand base) base))))

(defun rand (&optional (n 1)) 
  "Return a random float."
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

; ##  Start-up 
; ###  Egs
; Each of these `eg-xxx` functioncs can be called from 
; the command line with `-xxx` (with an optional arg).
(defun eg-one (_) (format t "~a~%" (? bayes k)))

(defun eg-mapread (f) (mapread #'print (? train) ))

(defun eg-data (&optional file) 
  (dolist (col (o (make-data (or file (? train))) cols y)) 
    (format t "~a~%" col)))

; ###  Main 
(defun main (args)
  "Ask the command line if there is any EG- function to run."
  (loop :for (flag arg) :on args :by #'cdr :do
    (let ((fun (intern (format nil "EG~:@(~a~)" flag))))
      (if (fboundp fun)
        (funcall fun (if arg (str2thing arg)))))))

(main #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*)
