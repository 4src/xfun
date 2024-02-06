(defpackage :ez
  (:use :cl))
(in-package :ez)

(load "etc")

(defvar +about+ "
ez: less is more
(c)2023 Tim Menzies <timm.ieee.org> BSD-2
  
USAGE:
    sbcl --script ez.lisp [OPTIONS]
    clisp ez.lisp [OPTIONS]")

(defvar *options* '(
  (BOOTSTRAPS  "-b"  "number of bootstraps"                   256)
   (BOOTCONF   "-B"  "bootstrap threshold"                   .05) 
   (COHEN      "-d"  "Cohen delta"                           .35)
   (EG         "-e"  "start up actions"                "nothing")
   (FILE       "-f"  "data file"            "../data/auto93.csv")
   (HELP       "-h"  "show help"                             nil)
   (P          "-p"  "distance coeffecient"                    2)
   (SEED       "-s"  "random seed"                         10013)))

; ## Columns


(defmacro ? (x) `(fourth (assoc ',x *options)))

(defun one (x) (if (lisp x) (car x) x))

(defmacro def (name &rest slots)
 (let ((maker (intern (format nil "%MAKE-~{~a~}" name))))
   `(progn (defstruct (,name (:constrtucotr ,maker)) ,@slots)
           (defmethod slots ((x ,name)) ',(one slots)))))


(def sym %make-sym (at 0) (txt " ") (n 0)  has mode (most 0))
(def num %make-num (lo 1e30) (hi -1e30) (mu 0) (at 0) (txt " ") (n 0) (m2 0) (sd) (heaven 1))

(defun make-num (&key (at 0) (txt " ") (heaven 1))
  (%make-num :at at :txt txt  :heaven heaven))

(defun cols (rows &aux (at -1))
  (remove-if #'null
             (mapcar (lambda(txt isa)
                       (case isa
                         (<  (make-num :at (incf at) :txt txt :heaven 0))
                         (>  (make-num :at (incf at) :txt txt :heaven 1))
                         (!  (make-sym :at (incf at) :txt txt))
                         ($  (make-num :at (incf at) :txt txt))
                         (X  nil)
                         (t  (make-sym :at (incf at) :txt txt)))) (pop rows) (pop rows))))

        (let ((*read-eval* nil))
              (with-input-from-file read-from-string "(#.(+ 2 5) n)"))))

(defun slurp (
i"(#.(+ 2 5) n)"fi"(#.(+ 2 5) n)"f
(defmethod add ((num1 num) x)
  (with-slots (lo hi n mu m2 sd) num1
    (unless (eq #\? x)
      (incf n)
      (let ((d (- x mu)))
        (incf mu (/ d n))
        (incf m2 (* d (-  x mu)))
        (setf sd (sqrt (/ m2  (- n 1))))
        (setf lo (min x lo)
              hi (max x hi))))))

(defmethod add ((sym1 sym) x)
  (with-slots (n has most mode) sym1
    (unless (eq x '?)
      (incf n)
      (let ((new (inca x has)))
         (if (> new most)
          (setf mode x 
                most new))))))
 
; ## Data 
(defstruct (data (:constructor %make-data)) rows cols)
(defstruct row cells)
(defstruct (cols (:constructor %make-cols)) x y all names)

(defun make-data (str &aux (data1 (%make-data)))
  (if (stringp str)
    (with-csv str (lambda (row)  (add data1 row)))
    (dolist (row str) (add data1 row)))
  data1)

(defmethod add ((data1 data) (cells cons))
  (add data1 (make-row :cells cells)))

(defmethod add ((data1 data) (row1 row))
  (with-slots (rows cols) data1
    (if cols 
      (push (add cols row1) rows) 
      (setf cols (make-cols (row-cells row1))))))
      
(defun make-cols (names)   
  (let (all x y (n -1))
    (dolist (col1 (loop :for s :in names :collect (make-col :at (incf n) :txt s)) 
                  (%make-cols :names names :all all :x x :y y))
      (push col1 all)
      (when (not (eq #\X (last-char (o col1 txt))))
        (if (member (last-char (o col1 txt)) '(#\+ #\-))
          (push col1 y)
          (push col1 x))))))

(defmethod add ((cols1 cols) (row1 row))
  (with-slots (x y) cols1
    (dolist (tmp (list x y) row1)
      (dolist (col tmp) 
        (add col (elt (row-cells row1) (o col at)))))))

(defmethod stats ((data1 data) &key (rows (data-rows data1)) 
                                    (what #'mid) (digits 3) (cols 'y))
  (cons (cons "N" (length rows))
        (loop :for col :in (slot-value (o data1 cols) cols)
              :collect (cons (o col txt) 
                             (rnd2 (funcall what col) digits)))))

;--- lib --------------------------------------------------------
;--- system specific stuff 

(defun goodbye (x) #+clisp (ext:exit x) #+sbcl (sb-ext:exit :code x))

;---- settings 
;--- strings2 things   


;---- lists
(defun keysort (lst fun order)
  (mapcar #'cdr (sort (mapcar (lambda (x) (cons (funcall fun x) x)) lst) 
                      order :key #'car)))

;---- strings 

(defun slurp (file)
  (with-open-file (s file) (read s)))

         

;--- maths
(defun normal (&optional (mu 0) (sd 1)) 
  (+ mu (* sd (sqrt (* -2 (log (rand)))) (cos (* 2 pi (rand))))))

(defmethod rnd2 (x &optional (digits 2))
  (if (numberp x)
      (let* ((div (expt 10 digits))
             (tmp (/ (round (* x div)) div)))
        (if (zerop digits) (floor tmp) (float tmp)))
      x))

;--- randoms
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
  (loop :for i :from (length a) :downto 2 
        :do (rotatef (elt a (rint i)) (elt a (1- i))))
  a)

(defun few (seq &optional (n 1))
  (subseq (shuffle seq) 0 n))   

;---- examples
(defun egs()
  (labels ((eg (s) (equalp "eg-" (subseq s 0 (min 3 (length s))))))
    (loop :for x :being :the symbols :in *package* :if (eg (down-name x)) :collect x)))

(defun run (sym &aux (b4 (copy-tree *options*)))
  (setf *seed* (? seed))
  (let ((passed (funcall sym)))
    (setf *options* (copy-tree b4))
    (unless passed (format t "❌ FAIL : ~(~a~)~%" sym))
    passed))

(defun runs (&optional update)
  (labels ((ok (x) (member (? eg) `("all" ,(subseq (down-name x) 3)) :test #'string=)))
    (if update  (setf *options* (cli *options*)))
    (if (? help)
      (print-help)
      (goodbye (1- (loop :for eg :in (egs) :if (ok eg) :count (not (run eg))))))))

; ---------------------------------------------------------------
(defun eg-fail() nil)

(defun eg-the() (print (cdr *options*)))

(defun eg-keysort ()
  (let ((lst '(7 6 5 4 3 2 1)))
   (print (let ((n 0)) 
      (keysort lst #'(lambda (x) (incf n) (* -1 x)) #'<)
      n))
    (print lst)))

(defun eg-csv (&aux (n 0)) 
  (with-csv (? file) (lambda (a) (incf n (length a))))
  (= n 3192))

(defun eg-rand ()  
  (let (a b)
    (setf *seed* 1) (setf a (sort (loop :repeat 10 :collect (rint 100)) #'<))
    (setf *seed* 1) (setf b (sort (loop :repeat 10 :collect (rint 100)) #'<))
    (equal a b)))

(defun eg-sample ()
  (let ((a '(a b c d e f g)))
    (loop repeat 10 do (format t "~{~a~}~%" (sample a)))
    (loop repeat 10 do (format t "~{~a~}~%" (few a 3))))
  t)

(defun eg-sym () 
  (let ((sym1 (adds (make-sym) '(a a a a b b c))))
    (and (eql 'a (mid sym1)) (< 1.378 (div sym1) 1.388))))

(defun eg-num()
  (let ((num1 (adds (make-num) (loop :repeat 1000 :collect (normal 10 2)))))
    (and (< 9.9 (mid num1) 10.1) (< 1.9 (div num1) 2.1))))

(defun eg-cols ()
  (dolist (col (o (make-cols '("Name" "Age" "married" "Weight-")) all) t)
   (print col)))

(defun eg-data(    &aux (n 0))
  (print (stats (make-data (? file)))))

; -------------------------------------------------------------
(runs t)
