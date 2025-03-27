;; [asdas](asdas) | [asdas](asdas) | [asdas](asdsa) 
;; <img align="right" width=100 src="head.png">
(defpackage :mink (:use :cl)) (in-package :mink)
#+sbcl (declaim (sb-ext:muffle-conditions cl:style-warning))

;; --------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
;; ## Settings

(defstruct about 
  (what  "mink.lisp")
  (why   "optimization via recursive kmeans")
  (when  "(c) 2024")
  (how   "MIT license")
  (who   "Tim Menzies")
  (where "timm@ieee.org"))

(defstruct bayes (m 1) (k 2))

(defstruct settings 
  (seed    1234567891)
  (buckets 2)
  (pp      2)
  (train   "data/auto93.lisp")
  (about  (make-about)) 
  (bayes  (make-bayes)))

(defvar *settings* (make-settings))

;; --------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
;; ## Macros

;; (o x slot1 slot2...) ==>    
;; (slot-value (slot-value (slot-value x 'slot1) 'slot2) ...)
(defmacro o (x f &rest fs) 
  (if fs
      `(o (slot-value ,x ',f) . ,fs)
      `(slot-value ,x ',f)))

;; (? x y ...) ==> o(*settings* x y ...)
(defmacro ? (&rest slots)
  `(o *settings* . ,slots))

;; $x ==> (slot-value it 'x)
;;(set-macro-character #\$ #'(lambda (s _) `(slot-value it ',(read s t nil t))))
(set-macro-character #\$ #'(lambda (s _) `(slot-value it ',(read s))))

;;---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
;; ## Structs

;; COLs are NUMs and SYMS have `name`; are found `at` some column index.
(defstruct (col)
   (n 0) (at 0) (name " "))

;; SYMs track a `count` of symbols. The `mode` is the `most` common symbol.
(defstruct (sym (:include col))
  mode (most 0) (count (make-hash-table :test #'equal)))

;; COLS are factories turning  strings to NUMs or COLs.
(defstruct (cols (:constructor %make-cols))
  names all x y)

;; 'rows' are summazied in 'cols'.
(defstruct (data (:constructor %make-data))
  rows cols)

;; NUMs tracks `lo`, `hi`,  mean `mu`, stdev `sd` seen so far.
(defstruct (num (:include col) (:constructor %make-num))
  (goal 1) (mu 0) (m2 0) (lo 1E32) (hi -1E32))

;;---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
;; ## Create

;; Create a data from either a list or a file or rows.
(defun make-data (&optional src &aux (it (%make-data)))
  (labels ((fun (rowcsv) (add it row)))
    (if (stringp src) 
      (mapcsv #'fun src) 
      (mapcar #'fun src)))
  it)

;; Create number, and set the `goal` from the `name`.
(defun make-num (&key (at 0) (name " ") &aux (it (%make-num :at at :name name)))
  (setf $goal (if (eql #\- (chr $name -1)) 0 1))
  it)

;; Create `num`eric or `sym`bolic columns from a list of `name` strings.
(defun make-cols (names &aux (it (%make-cols :names names)))
  (dolist (name names it)
    (labels ((what  ()   (if (upper-case-p (chr name 0)) #'make-num #'make-sym))
             (make (fun) (funcall fun :name name :at (length $all)))
             (keep (col) 
                   (push col $all)
                   (unless (eql (chr name -1) #\X) 
                     (if (member (chr name -1) (list #\! #\- #\+)) 
                       (push col $y)
                       (push col $x)))))
      (keep (make (what))))))

;; --------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
;; ## Add 

;; First time, create columns. Next, summarize `row` in `cols` and store in `rows`.
(defmethod add ((it data) (row cons))
  (if $cols
    (push (add $cols row) $rows)
    (setf $cols (make-cols row))))

;; Summarize a row in the `x` and `y` columns.
(defmethod add ((it cols) row)
  (dolist (col $x) (add col (of col row)))
  (dolist (col $y) (add col (of col row))))

;; Summarize `x` in a column (unless it is don't know.
(defmethod add ((it col) x)
  (unless (eq x '?)
     (incf $n)
     (add1 it x)))

;; Update a NUM.
(defmethod add1 ((it num) (x number)) 
  (let ((d (- x $mu)))
    (incf $mu (/ d $n))
    (incf $m2 (* d (-  x $mu)))
    (setf $lo (min x $lo)
          $hi (max x $hi))))

;; Update a SYM.
(defmethod add1 ((it sym) x) 
  (let ((new (incf (gethash x $count 0))))
    (if (> new $most)
      (setf $mode x
            $most new))))

;; --------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
;; ## Misc

;; Cell access.
(defmethod of ((it col) row) (elt row $at))

;; Middle of a distribution.
(defmethod mid ((it num)) $mu)   
(defmethod mid ((it sym)) $mode) 

;; NUMbers have standard deviation.
(defmethod div ((it num)) 
  (if (< $n 2) 0 (sqrt (/ $m2 (- $n 1)))))

;; SYMbols have entropy.
(defmethod div ((it sym)) 
  (- (loop :for v :being the hash-values of $count :sum (* (/ v $n) (log (/ v $n) 2)))))

;; --------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
;; ## Bayes 

;; Return the log likelihood of a row.
(defmethod loglike ((it data) row &key nall nh) 
  (labels ((num (n) (if (< n 0) 0 (log n))))
    (let* ((prior (/ (+ (length $rows) (? bayes k)) 
                     (+ nall (* nh (? bayes k))))))
      (+ (num prior) (loop :for col :in (o $cols x) :if (not (equal '? (of col row)))
                           :sum (num (like col (of col row) :prior prior)))))))

;; Return likelhood of a SYMbol.
(defmethod like ((it sym) x &key prior) 
  (/ (+ (count $seen x) (* (? bayes m) prior)) 
     (+ $n (? bayes m))))

;; Return likelhood of a NUMber.
(defmethod like ((it num) x &key prior) 
  (let ((sd (+ (div it)  1E-30)))
    (/ (exp (- (/ (expt (- x $mu) 2) (* 2 (expt sd 2)))))
       (* sd (sqrt (* 2 pi))))))

;; --------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
;; ##  Utils 
;; ### Strings

;; Return character from a string (knows how to handle negatives).
(defun chr (s n)
  (char s (+ n (if (>= n 0) 0 (length s)))))

;; Coerce `s` to an atomic thing.
(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s))) 
  (let ((x (let ((*read-eval* nil)) (read-from-string s1 ""))))
    (if (or (numberp x) (member x '(t nil ?)))
      x
      s1)))

;; Split string to items, divided on some `sep` character; then coerce each item.
(defun things (s &optional (sep #\,) (here 0)) ; --> list
  (let ((there (position sep s :start here)))
    (cons (thing (subseq s here there))
          (if there (things s sep (1+ there))))))

;; ### Files

;; Call `fun` on all lines in `file`. Returns when we read nil (at eof).
(defun mapcsv (&optional (fun #'print) file end)
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (things (or (read-line s nil)
                                   (return end)))))))
       
;; Run `fun` for all things in a file.
(defun mapfile (fun file)
  (with-open-file (s file) 
    (loop (funcall fun (or (read s nil nil) (return))))))

;; ### Random numbers

;; Enables platform-independent seeding for random nums in LISP."
(defvar *seed* (? seed))

;; Return a random integer.
(defun rint (&optional (n 100) &aux (base 1E10)) 
  (floor (* n (/ (rand base) base))))

;; Return a random float. 
(defun rand (&optional (n 1)) 
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

;; List a list of command-line options.
(defun args()
  (cdr #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*))

;; --------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
;; ##  Start-up 
;; ###  Egs

;; Each of these `eg-xxx` functions can be called from 
;; the command line with `-xxx` (with an optional arg).
(defun eg--one (_) (format t "~a~%" (? bayes k)))

(defun eg--mapread (f) (mapfile #'print (? train) ))

(defun eg--data (&optional file)
  (let ((data (make-data (or file (? train)))))
    (dolist (col (o data cols y))
      (format t "~a~%" col))))

;; ###  Main
(loop :for (flag arg) :on (args) :by #'cdr 
      :do  (let ((com (intern (format nil "EG~:@(~a~)" flag))))
             (when (fboundp com)
                (setf *seed* (? seed))
                (funcall com (if arg (thing arg))))))
