; <!-- vim: set ts=2 sw=2 sts=2 et showmatch: -->
;(defpackage :ezr (:use :cl))
;(in-package :ezr)

#+sbcl
(declaim (sb-ext:muffle-conditions cl:style-warning))

(defstruct about 
  "Struct for file meta info."
  (what  "mink.lisp")
  (why   "optimization via recursive kmeans")
  (when  "(c) 2024")
  (how   "MIT license")
  (who   "Tim Menzies")
  (where "timm@ieee.org"))

(defstruct bayes
 (m 1)
 (k 2))

(defstruct settings 
  "Struct for all settings."
  (seed    1234567891)
  (buckets 2)
  (pp      2)
  (train   "data/auto93.lisp")
  (about  (make-about))
  (bayes  (make-bayes)))

(defvar *settings* (make-settings))

;---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
(defstruct (data (:constructor %make-data))
  "'rows' are summazied in 'cols'."
  rows cols)

(defstruct (cols (:constructor %make-cols))
  "COLS are factories. Turns strings to NUMs or COLs (may be `x` inpits or `y` goals)."
  names all x y)

(defstruct col
  "COLs are NUMs and SYMS have `name`; are found `at` some column index."
  (n 0) (at 0) (name " "))

(defstruct (sym (:include col))
  "SYMs track a `count` of symbols. The `mode` is most common symbol count."
  count mode (most 0))

(defstruct (num (:include col) (:constructor %make-num))
  "NUMs track the `lo` and `hi` seen so far; as well as mean and stdev (`mu`m `sd`)."
  (goal 1) (mu 0) (m2 0) (sd 0) (lo 1E32) (hi -1E32))

;---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
(defmacro o (x f &rest fs) 
  (if fs `(o (slot-value ,x ',f) ,@fs) `(slot-value ,x ',f)))

(defmacro ? (&rest slots) 
  `(o *settings* ,@slots))

(set-macro-character
   #\$ #'(lambda (s _) `(slot-value self ',(read s t nil t))))
 
;---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------   
(defun make-num(&key (at 0) (name " ") &aux (self (%make-num :at at :name name)))
  (setf $goal (if (eql #\- (last-char $name)) 0 1))
  self)

(defun make-cols (names &aux (self (%make-cols :names names)))
  (dolist (name names self) ; return self
    (let* ((a   (char name 0)) 
           (z   (last-char name))
           (fun (if (upper-case-p a) #'make-num #'make-sym))
           (col (funcall fun :name name :at (length $all))))
      (push col $all)
      (unless (eql z #\X) 
        (if (member z (list #\! #\- #\+)) (push col $y) (push col $x))))))

(defun make-data (&optional src &aux (self (%make-data)))
  (labels ((fun (row) (add self row)))
    (if (stringp src) (mapfile #'fun src) (mapcar #'fun src)))
  self)

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
  (let ((new (incf (cdr (or (assoc x $count :test #'equal) 
                            (car (setf $count (cons (cons x 0) $count))))))))
    (if (> new $most)
        (setf $mode x
              $most new))))

 ; ### Misco
(defmethod cell ((self col) row) (elt row $at))

(defmethod mid ((self num)) $mu)   
(defmethod mid ((self sym)) $mode) 

(defmethod div ((self num)) ; --> float
  "NUMbers have standard deviation"
  (if (< $n 2) 0 (sqrt (/ $m2 (- $n 1)))))

(defmethod div ((self sym)) ; --> float
  "SYMbols have entropy"
  (* -1 (loop :for (_ . v) :in $has :sum (* (/ v $n) (log (/ v $n) 2)))))

; ###  Bayes 
(defmethod loglike ((self data) row &key nall nh) ; --> float
  "Return the log likelihood of a row."
  (labels ((num (n) (if (< n 0) 0 (log n))))
    (let* ((prior (/ (+ (length $rows) (? bayes k)) 
                     (+ nall (* nh (? bayes k))))))
      (+ (num prior) (loop :for col :in (o $cols x) 
                           :if (not (equal '? (cell col row)))
                           :sum (num (like col (cell col row) :prior prior)))))))

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
(defun mapfile (fun file)
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

(defun eg(s)
 (intern (format nil "EG~a" (string-upcase s))))

(defun args()
  #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*)

; ##  Start-up 
; ###  Egs
; Each of these `eg-xxx` functioncs can be called from 
; the command line with `-xxx` (with an optional arg).
(defun eg--one (_) (format t "~a~%" (? bayes k)))

(defun eg--mapread (f) (mapfile #'print (? train) ))

(defun eg--data (&optional file) 
  (dolist (col (o (make-data (or file (? train))) cols y)) 
    (format t "~a~%" col)))

(loop :for (flag arg) :on (args) :by #'cdr :if  (fboundp (eg flag)) 
      :do  (funcall (eg flag) (if arg (str2thing arg))))
