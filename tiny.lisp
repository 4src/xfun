; vim : set ts=3 sw=3 sts=3 et :
(print 1)
(defpackage :tiny (:use :cl))
(in-package :tiny)

(print 1)
;## Globals
(defvar *settings* ; car is help text, cdr are the settings
'("
tiny : fun with stuff
(c)2023 Tim Menzies <timm.ieee.org> BSD-2

USAGE :
sbcl --script tiny.lisp [OPTIONS] -e [ACTIONS]
"
    (bootstraps "-B"    "number of bootstraps"  256)
    (bootConf   "--CB"  "bootstrap threshold"   .05)
    (cliffs     "--CC"  "cliffs delta"          .147)
    (cohen      "-c"    "cliffs delta"          .35)
    (eg         "-e"    "start up actions"      "nothing")
    (file       "-f"    "data file"             "../data/auto93.csv")
    (help       "-h"    "show help"             nil)
    (p          "-p"    "distance coefficient"  2)
    (seed       "-s"    "random seed"           1234567891)
    ))
;-----------------------------------------------------------------------------------------
(defmacro ? (x) `(cadddr (assoc ',x  (cdr *settings*))))

(defmacro aif (test then &optional else) 
  `(let ((it ,test)) 
     (if it ,then ,else)))

(defmacro o (struct slot &rest slots)
  (if slots 
    `(o (slot-value ,struct ',slot) ,@slots)  
    `(slot-value ,struct ',slot)))  

(defmacro seen (x lst &optional (init 0))
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))
;-----------------------------------------------------------------------------------------
(defun args ()
  #+clisp ext:*args*
  #+sbcl sb-ext:*posix-argv*)

(defun goodbye (&optional (x 0))
  #+clisp (ext:exit x)
  #+sbcl  (sb-ext:exit :code x))

(defun round2 (number &optional (digits 2))
  "round to `digits` number of decimal places"
  (let* ((div (expt 10 digits))
         (tmp (/ (round (* number div)) div)))
    (if (zerop digits) (floor tmp) (float tmp))))

(defvar *seed* 10013)
(defun rand (&optional (n 1))
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 1) &aux (base 10000000000.0))
  (floor (* n (/ (rand base) base))))

(defmethod sample ((a cons) &optional (n (length a))) 
  (sample (coerce a 'vector) n))

(defmethod sample ((a vector) &optional (n (length a)))
  (let ((len (length a)))
    (loop :repeat n :collect (elt a  (rint len)))))

(defmethod shuffle ((a cons)) (coerce (shuffle (coerce a 'vector)) 'cons))
(defmethod shuffle ((a vector)) 
  (loop :for i :from (length a) :downto 2 :do (rotatef (elt a (rint i)) (elt a (1- i))))
  a)

(defun pooled (i j) 
   (sqrt (/ (+ (* (1- (len i)) (expt (div i) 2))
               (* (1- (len j)) (expt (div j) 2)))
            (+ (len i) (len j)  -2))))

(defun few (seq n)
   (subseq (shuffle seq) 0 n))

(defun time-it (fun &optional (repeats 1))
  (let ((t0 (get-internal-real-time)))
    (dotimes (_ repeats) (funcall fun))
    (float (/ (-(get-internal-real-time) t0) repeats))))

(defun normal (&optional (mu 0) (sd 1)) 
  (+ mu (* sd (sqrt (* -2 (log (rand)))) (cos (* 2 pi (rand))))))

(defmethod last-char ((s string)) (char s (1- (length s))))
(defmethod last-char ((s symbol)) (last-char (symbol-name s)))

(print 1)
(defun cli (lst)
   (loop :for (key flag help b4) :in lst :collect
        (list key flag help (aif (member flag (args) :test #'string=)
                                 (cond ((eq b4 t) nil)
                                       ((eq b4 nil) t)
                                       ((stringp b4) (second it))
                                       (t  (read-from-string (second it))))
                                 b4))))

(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  (if (string= s1 "?") #\?
    (let ((x (read-from-string s1)))
      (cond ((numberp x) x)
            ((eq x t)    x)
            ((eq x nil)  x)
            (t           s1)))))

(defun split (s &optional (sep #\,) (filter #'thing) (here 0))
  (let* ((there (position sep s :start here))
         (word  (funcall filter (subseq s here there))))
    (labels ((tail () (if there (split s sep filter (1+ there)))))
      (if (equal word "") (tail) (cons word (tail))))))

(defun with-lines (file fun &optional (filter #'split))
  (with-open-file (s (or file  *standard-input*))
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))

(defun per (a &optional (p .5)) (elt a (floor (* p (length a)))))

(defmethod div ((a cons)) (/ (- (per a .9) (per a .1)) 2.56))
(defmethod mid ((a cons)) (per a .5))
(defmethod len ((a cons)) (length a))

(defmethod cohen ((xs cons) (ys cons) &optional sorted)
  (unless sorted
    (setf xs (sort xs #'<)
          ys (sort ys #'<)))
  (> (/ (abs (- (mid xs) (mid ys))) 
        (pooled xs ys)) 
     (? cohen)))

(defmethod cliffs-delta ((xs cons) (ys cons) &aux (n12 0) (lt 0) (gt 0) 
                                (n1 (length xs)) (n2 (length ys)))
  (cond ((> n1 (* 10 n2)) (cliffs-delta (few xs (* 10 n2)) ys))
        ((> n2 (* 10 n1)) (cliffs-delta xs (few ys (* 10 n1))))
        (t (dolist (x xs)
             (dolist (y ys)
               (incf n12)
               (if (> x y) (incf gt))
               (if (< x y) (incf lt))))
           (> (/ (abs (- gt lt)) n12) (? cliffs)))))

(defun critical (i j)
  (/ (abs (- (mid i) (mid j)))
     (sqrt (+ (/ (expt (div i) 2) (o i n)) 
              (/ (expt (div j) 2) (o j n))
              1E-30))))

(defmethod bootstrap ((y0 cons) (z0 cons))
  (let* ((y    (adds (make-num) y0))
         (z    (adds (make-num) z0))
         (x    (adds (adds (make-num) y0) z0))
         (d    (critical y z))
         (yhat (mapcar (lambda(y1) (- y1 (mid y) (mid x))) y0))
         (zhat (mapcar (lambda(z1) (- z1 (mid z) (mid x))) z0))
         (n    (loop :repeat (? bootstraps)
                     :count  (> (critical (adds (make-num) (sample yhat)) 
                                          (adds (make-num) (sample zhat))) 
                                d)))) 
    (< (/ n (? bootstraps)) (? bootConf))))

(defun different (xs ys)
  (and (cliffs-delta xs ys) (bootstrap xs ys)))
;-----------------------------------------------------------------------------------------

(defstruct sym 
  (at 0) (name " ") (n 0) has (most 0) mode)

(defmethod mid ((s sym)) (sym-mode s))
(defmethod div ((s sym))
  (with-slots (has n) s
    (* -1  (loop :for (_ . v) :in has :sum  (* (/ v n)  (log (/ v n) 2))))))

(defstruct (num (:constructor %make-num)) 
  (at 0) (name " ") (n 0) (mu 0) (m2 0) (lo 1E30) (hi -1E30) (heaven 0))

(defun make-num (&key (at 0) (name " "))
  (%make-num :at at :name name :heaven (if (eq #\- (last-char name)) 0 1)))

(defmethod mid ((n num)) (float (num-mu n)))
(defmethod div ((n num)) (sqrt (/ (num-m2 n) (- (num-n n) 1))))
(defmethod len ((n num)) (num-n n))

(defmethod norm ((n num) x)
  (with-slots (lo hi) n
    (if (eq x #\?) x (/ (- x lo) (- hi lo 1E-30)))))

(defun make-col (&key (at 0) (name " "))
  (if (upper-case-p (char name 0))
    (make-num :at at :name name)
    (make-sym :at at :name name)))

(defstruct (cols (:constructor %make-cols))
   x y all names)

(defun make-cols (lst &aux (n -1))
  (let ((cols1 (%make-cols 
                 :names lst
                 :all (mapcar (lambda (s) (make-col :at (incf n) :name s)) lst))))
    (dolist (col (reverse (o cols1 all)) cols1)
      (when (not (eq #\X (last-char (o col name))))
        (if (member (last-char (o col name)) '(#\+ #\-))
          (push col (o cols1 y))
          (push col (o cols1 x)))))))

(defstruct row cells)
(defmethod cell ((self row) col)
  (elt (row-cells self) (o col at)))

(defstruct (sheet (:constructor %make-sheet))  rows cols)
(defun make-sheet (src &optional (self (%make-sheet)))
  (adds self src))


(defmethod add ((self sheet) (row1 row))
  (with-slots (rows cols) self
    (if cols 
      (progn (push row1 rows)
             (mapcar #'add (o cols all) (o row1 cells)))
      (setf cols (make-cols (row-cells row1))))))

(defmethod add ((self num) x)
  (with-slots (lo hi n mu m2) self
    (unless (eq #\? x)
      (incf n)
      (let ((d (- x mu)))
        (incf mu (/ d n))
        (incf m2 (* d (-  x mu)))
        (setf lo (min x lo)
              hi (max x hi))))))

(defmethod add ((self sym) x)
  (with-slots (mode most n has) self
    (unless (eq #\? x)
      (incf n)
      (if (> (incf (seen x has)) most)
        (setf most (cdr (assoc x has))
              mode x)))))

(defmethod adds ((self sheet) (file string))
  (with-lines file (lambda (lst) (add self (make-row :cells lst))))
  self)

(defmethod adds (self (lst cons))
  (dolist (item lst self) (add self item)))

;-----------------------------------------------------------------------------------------
(defmethod dist ((s sym) x y)
  (if (and (eq x #\?) (eq y #\?)) 
    1
    (if (equal x y) 0 1)))

  (defmethod dist ((n num) x y)
    (if (and (eq x #\?) (eq y #\?)) 
      1
      (let ((x (norm n x))
            (y (norm n y)))
        (if (equal x #\?) (setf x (if (< y .5) 1 0) x))
        (if (equal y #\?) (setf y (if (< x .5) 1 0) y))
        (abs (- x y)))))

(defmethod dist ((self sheet) (x row) (y row))
  (labels ((gap (col x y) (dist col (cell x col) (cell y col))))
    (let* ((tmp (loop for col in (o self cols) sum (expt (gap col x y) (? p)))))
      (expt (/ tmp (length (o self cols))  (/ 1 (? p)))))))
;-----------------------------------------------------------------------------------------
(defun eg-fail()
  "can the test engine handle a fail?"
   (< 1 0))

(defun eg-crash()
  "can the test engine handle a crash>"
   (fred 3 0))

(defun eg-set () 
  "are the settings ok?"
  (format t "~{~a~%~}" *settings*)
  (dolist (x '(help seed file eg) t)
    (or (cdr (assoc x (cdr *settings*))) 
        (return-from eg-set (format t "missing in *settings* : ~a~%" x)))))

(defun eg-rand () 
  "if seed reset, then same psuedoi-randoms?"
  (let (a b)
    (setf *seed* 1) (setf a (sort (loop :repeat 10 :collect (rint 100)) #'<))
    (setf *seed* 1) (setf b (sort (loop :repeat 10 :collect (rint 100)) #'<))
    (equal a b)))

(defun eg-sample ()
  "can we sample with/out replacement?"
  (let ((a '(a b c d e f g)))
    (loop repeat 10 do (format t "~{~a~}~%" (sample a)))
    (loop repeat 10 do (format t "~{~a~}~%" (few a 3))))
  t)

(defun eg-file (&aux (n 0))
  "can count cells in a csv file?"
  (with-lines (? file) (lambda (a) (incf n (length a))))
  (= n 3192))

(defun eg-sym ()
  "can compute entropy?"
  (let ((sym1 (adds (make-sym) '(a a a a b b c))))
    (and (eql 'a (mid sym1)) (< 1.378 (div sym1) 1.388))))

(defun eg-num()
  "can compute mu and standard deviation?"
  (let ((num1 (adds (make-num) (loop :repeat 10000 :collect (normal 10 2)))))
    (and (< 9.95 (mid num1) 10.05) (< 1.95 (div num1) 2.05))))

(defun eg-shuffle ()
  "can numbers be shuffled?"
  (let ((nums (loop :for x :upto 20 :collect x)))
    (equal nums  (sort (shuffle (copy-tree nums)) #'<))))

(defun eg-stats ()
  (labels ((yn (x) (if x "." "="))
           (say (i &rest lst) (format t "~,2f ~{~,8T~a~}~%" i (mapcar #'yn lst))))
    (format t "~a ~{~,8T~(~a~)~}~%" 'b/a '(cliffs boot c+b cohen))
    (loop for i from 0 to 1.5 by .1 do 
          (let* ((r 256)
                 (sd .5) 
                 (mu 10)
                 (a (loop :repeat r collect (normal mu sd)))
                 (b (loop :repeat r collect (normal (+ i mu) (* 4 sd)))))
            (say i (cliffs-delta a b) (bootstrap a b) (different a b) (cohen a b))))
    (rand)))

(defun eg-cols()
  "can we create columns from a list of names?"
  (mapcar #'print (cols-x (make-cols '("name" "Age" "HeightX")))))

(defun eg-sheet ()
  (let ((s (make-sheet (? file))))
    (print (mapcar #'mid (o s cols y)))))
;-----------------------------------------------------------------------------------------
(defun tiny-run (sym &aux (b4 (copy-tree *settings*)))
  (setf *seed* (?  seed))
  (prog1
    (or (handler-case (funcall sym) 
          (error (c) (format t "~&✋ CRASH on ~a. ~a~%" sym c)))
        (format *error-output* "~&❌ FAIL: ~a~%" sym))
    (setf *settings* (copy-tree b4))))

(defun tiny-help (egs &optional (w 3))
  (format t "~a~%OPTIONS:~%" (car *settings*))
  (loop :for (_ s1 s2 __) :in (cdr *settings*) :do (format t "  ~10a  ~a~%" s1 s2))
  (format t "~%ACTIONS:~%")
  (loop :for x :in egs :do
     (format t "  -e ~8a ~a~%" (subseq (str x) w) (documentation x 'function))))

(defun tiny-main (&optional (pre "eg-") (w 3))
  (labels ((str  (sym) (string-downcase (symbol-name sym)))
           (eg   (x)   (equalp pre (subseq (str x) 0 (min w (length (str x)))))) 
           (egs  ()    (loop :for x :being :the symbols :in *package* :if (eg x) :collect x))
           (use  (x)   (member (? eg) `("all" ,(subseq (str x) w)) :test #'string=))
           (uses (lst) (loop :for x :in lst :if (use x) :collect x)))
    (setf (cdr *settings*) (cli (cdr *settings*)))
    (if (? help)
      (tiny-help (egs) w)
      (goodbye (loop :for eg :in (uses (egs)) :count (not (tiny-run eg)))))))

(tiny-main)
