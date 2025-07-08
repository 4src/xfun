;; [asdas](asdas) | [asdas](asdas) | [asdas](asdsa)    
;; <img align="right" width=100 src="head.png">
;; Note: in this code `i` is reserced to denote `self`.
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
  (train   "data/auto93.csv")
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

;; $x ==> (slot-value i 'x)
(set-macro-character 
  #\$ #'(lambda (s _) `(slot-value i ',(read s))))

;; Anaphoric if. Binds the result of TEST to IT in THEN."
(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))

;;---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
;; ## Structs

;; COLs are NUMs and SYMS have `name`; are found `at` some column index.
(defstruct (col)
   (n 0) (at 0) (name " "))

;; SYMs track a `count` of symbols.
(defstruct (sym (:include col))
  count mode (most 0))

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
(defun make-data (&optional src &aux (i (%make-data)))
  (labels ((fun (row) (add i row)))
    (if (consp src)
      (mapcar #'fun src)
      (mapcsv #'fun src))
    i))

;; Create number, and set the `goal` from the `name`.
(defun make-num (&key (at 0) (name " ") &aux (i (%make-num :at at :name name)))
  (setf $goal (if (eql #\- (chr $name -1)) 0 1))
  i)

;; Create `num`eric or `sym`bolic columns from a list of `name` strings.
(defun make-cols (names &aux (i (%make-cols :names names)))
  (dolist (name names i)
    (labels ((keep (col)
                   (push col $all)
                   (unless (eql (chr name -1) #\X) 
                     (if (member (chr name -1) (list #\! #\- #\+)) 
                       (push col $y)
                       (push col $x)))))
      (keep (funcall (if (upper-case-p (chr name 0)) #'make-num #'make-sym)
                     :name name :at (length $all))))))

;; --------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
;; ## Add 

;; First time, create columns. Next, summarize `row` in `cols` and store in `rows`.
(defmethod add ((i data) (row cons))
  (if $cols
    (push (add $cols row) $rows)
    (setf $cols (make-cols row))))

;; Summarize a row in the `x` and `y` columns.
(defmethod add ((i cols) (row cons))
  (dolist (cols (list $x $y))
    (dolist (col cols) 
      (add col (of col row)))))

;; Summarize `x` in a `num` (unless it is don't know.
(defmethod add ((i num) x)
  (unless (eq x '?)
    (incf $n)
    (let ((d (- x $mu)))
      (incf $mu (/ d $n))
      (incf $m2 (* d (-  x $mu)))
      (setf $lo (min x $lo)
	    $hi (max x $hi)))))

;; Summarize `x` in a `sym` (unless it is don't know.
(defmethod add ((i sym) x) 
  (unless (eq x '?)
    (incf $n)
    (let ((new (inc-alist $count x)) )
       (if (> new $most)
	 (setf $most new
	       $mode x)))))

;; Incremement an alist, return the new value
(defun inc-alist (alist  x &optional (n 1))
  (incf (cdr (or (assoc x alist :test #'equal) 
		 (car (setf alist (cons (cons x 0) alist))))) n))

;(defmacro has (x lst); then inc using (incf (has 'x list))
; `(cdr (or (assoc ,x ,lst :test #'equal)
;           (car (push (cons ,x 0) ,lst)))))


;; --------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
;; ## Merge
(defmethod fuse ((s1 sum) (s2 sym))
  (let ((it (copy-sym s2)))
    (loop for (k . v) in (o 2 counts) do (inc-alist $count v))
    (let ((two (car (sort $count $> :key #'cdr))))
      (set $most (car two)
	   $mode (cdr two)))))

(defmethod fuse ((i num) (j num))
  (let ((out (make-num :name $name :at $at)))
    (with-slots     (n  mu  m2  lo  hi) out
      (with-slots   (n1 mu1 m21 lo1 hi1) i
	(with-slots (n2 mu2 m22 lo2 hi2) j
	  (setf lo (min lo1 lo2)
		hi (max hi1 hi2)
		n  (+ n1 n2)
		mu (/ (+ (* n1 mu1) (* n2 mu2)) (+ n1 n2)) 
		m2 (+ m21 m22 (* n1 (/ n2 (+ n1 n2)) (- mu1 mu2) (- mu1 mu2)))))))
    out))

;; --------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
;; ## Misc

;; Cell access.
(defmethod of ((i col) row) (elt row $at))

;; Middle of a distribution.
(defmethod mid ((i num)) $mu)   
(defmethod mid ((i sym)) $mode)

;; NUMbers have standard deviation.
(defmethod div ((i num)) 
  (if (< $n 2) 0 (sqrt (/ $m2 (- $n 1)))))

;; SYMbols have entropy.
(defmethod div ((i sym)) 
  (- (loop :for v :being the hash-values of $count :sum (* (/ v $n) (log (/ v $n) 2)))))

;; --------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
;; ## Bayes 

;; Return the log likelihood of a row.
(defmethod loglike ((i data) row &key nall nh) 
  (labels ((num (n) (if (< n 0) 0 (log n))))
    (let* ((prior (/ (+ (length $rows) (? bayes k)) 
                     (+ nall (* nh (? bayes k))))))
      (+ (num prior) (loop :for col :in (o $cols x) :if (not (equal '? (of col row)))
                           :sum (num (like col (of col row) :prior prior)))))))

;; Return likelhood of a SYMbol.
(defmethod like ((i sym) x &key prior) 
  (/ (+ (count $seen x) (* (? bayes m) prior)) 
     (+ $n (? bayes m))))

;; Return likelhood of a NUMber.
(defmethod like ((i num) x &key prior) 
  (let ((sd (+ (div i)  1E-30))) (/ (exp (- (/ (expt (- x $mu) 2) (* 2 (expt sd 2)))))
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

(defmethod print-object ((i hash-table) stream)
  (labels ((pair (k) (format nil "~a => ~a"  k (gethash k i))))
    (format stream "{~{~A~^, ~}}" (mapcar #'pair (keys i)))))

(defmethod keys ((i hash-table))
  (sort (loop :for k :being the hash-keys :of i :collect k) 
	#'string< :key #'princ-to-string))

;; ### Files

;; Call `fun` on all lines in `file`. Returns when we read nil (at eof).
(defun mapcsv (&optional (fun #'print) file end)
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (things (or (read-line s nil)
                                   (return end)))))))
       
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

(defun eg--csv (f) (mapcsv #'print (? train) ))

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
