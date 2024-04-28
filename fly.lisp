(defpackage :fly (:use :cl))
(in-package :fly)

(defvar  *options* '(
  (k     "-k"  "kth value"        2)
  (goal  "-g"  "start-up action"  "one")
  (help  "-h"  "show help"        nil)))
; ---------------------------------------------------------------------------------------
(defun goodbye (x)   #+clisp (ext:exit x) #+sbcl (sb-ext:exit :code x))

(defmacro ? (x) `(fourth (assoc ',x *options*)))
(defmacro has (x lst)  `(cdr (or (assoc ,x ,lst :test #'equal)
                                 (car (setf ,lst (cons (cons ,x 0) ,lst))))))

; ---------------------------------------------------------------------------------------
(defstruct sym (n 0) (at 0) (txt " ") (has 0) most mode)
(defstruct num (n 0) (at 0) (txt " ") (mu 0) (m2 0) (sd 0) (heaven 1))
(defstruct data rows cols fun)
(defstruct cols x y all names klass)

;-----------------------------------------------------------------------------------------
(defun new-sym (&optional (at 0) (s " ")) (make-sym :at 0 :txt s ))
(defun new-num (&optional (at 0) (s " ")) 
  (make-num :at 0 :txt s :heaven (if (eq (end s) #\-) 0 1)))

; ---------------------------------------------------------------------------------------
(defmethod add ((sym1 sym) x)
  (with-slots (n has most mode) sym1  (unless (eq x '?)
      (incf n)
      (let ((new (incf (has x has))))
        (if (> new most)
          (setf mode x 
                most new))))))

(defmethod add ((num1 num) x)
  (with-slots (lo hi n mu m2) num1
    (unless (eq x '?)
      (incf n)
      (let ((d (- x mu)))
        (incf mu (/ d n))
        (incf m2 (* d (-  x mu)))
        (setf lo (min x lo)
              hi (max x hi))))))

(defmethod add ((data1 data) row)
  (with-slots (rows cols fun) data1
    (cond (cols (funcall (data-fun data1) data1 row)
                (push (add cols row) rows))
          (t    (setf cols (new-cols row))))))

; ---------------------------------------------------------------------------------------
(defmethod mid ((num1 num)) (num-mu num1))
(defmethod mid ((sym1 sym)) (sym-mode sym1))

(defmethod div ((num1 num)) (sqrt (/ (num-m2 num1) (- (num-n num1) 1))))
(defmethod div ((sym1 sym))
  (with-slots (has n) sym1
    (* -1 (loop :for (_ . v) :in has :sum  (* (/ v n) (log (/ v n) 2))))))

;-----------------------------------------------------------------------------------------
(defun new-cols (lst &aux (n -1) (cols1 (make-cols :names lst)))
  (with-slots (x y all klass) cols1
    (dolist (s lst cols1)
      (incf n)
      (let* ((col (if (upper-case-p (char s 0)) (new-num n s) (make-sym :at n :txt s))))
        (push col all)
        (unless (eql (end s) #\X)
          (if (eql (end s) #\!) (setf klass col))
          (if (member (end s) '(#\< #\> #\!)) (push col y) (push col x)))))))

(defmethod add ((cols1 cols) lst) 
  (mapcar #'(lambda (c x) (add c x) x) (cols-all cols1) lst))

;-----------------------------------------------------------------------------------------
(defun new-data (lst &optional (fun (lambda (&rest _) _)) 
                     &key rank &aux (data1 (make-data :fun fun)))
  (with-slots (rows) data1
    (dolist (row lst) (add data1 row))
    (if rank (setf rows (sort rows #'< :key (lambda (row) (d2d data1 row)))))
    data1))

(defmethod d2h ((data1 data) lst)
  (let ((d 0) 
        (ys (cols-y (data-cols data1))))
    (dolist (col ys (expt (/ d (length ys)) .5)) 
      (with-slots (heaven at) col
        (incf d (expt (abs (- heaven (norm col (elt lst at)))) 2))))))

;------------------------------------------------------------------------------
(defun adds (col1 lst) (dolist (x lst col1) (add col1 x)))
(defun slurp (file)    (with-open-file (s file) (read s)))
(defun end(s)          (char s (1- (length s))))
(defun args ()         #+clisp ext:*args*  #+sbcl sb-ext:*posix-argv*)

(defun str2thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  (let ((*read-eval* nil)) (read-from-string s1 "")))

(defun cli (options &aux it)
  (loop :for (key flag help b4) :in options 
        :collect (list key flag help (if (setf it (member flag (args) :test #'string=))
                                         (cond ((eq b4 t) nil)
                                               ((eq b4 nil) t)
                                                (t (str2thing (second it))))
                                         b4))))

(defvar *seed* 10013)
(defun rand (&optional (n 1))
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 1) &aux (base 1E10)) (floor (* n (/ (rand base) base))))

;------------------------------------------------------------------------------
(defun egs ()
  (labels ((eg (x) (equal "eg-" (subseq (format nil "~(~a~)   " (symbol-name x)) 0 3))))
    (loop :for x :being :the symbols :in (find-package :fly) :if (eg x) :collect x)))

(defun run (sym &aux (b4 (copy-tree *options*)))
  (setf *seed* (? seed))
  (let ((passed (funcall sym)))
    (setf *options* (copy-tree b4))
    (unless passed (format t "❌ FAIL: ~(~a~)~%" sym))
    passed))

(defun fly-main (&optional update)
  (labels ((down (x) (string-downcase (symbol-name x)))
           (ok   (x) (member (? goal) `("all" ,(subseq (down x) 3)) :test #'string=)))
    (if update  (setf *options* (cli *options*)))
    (print *options*)
    (if (? help)
      (print-help)
      (goodbye (1- (loop :for eg :in (egs) :if (ok eg) :count (not (run eg))))))))  

;------------------------------------------------------------------------------
;(defun eg-one () (print 1))
(defun eg-ramd () (print 2)
  (let ((n (new-num)))
    (dotimes (i 1000) (print 0) (add n i))
    (print n)))
    
;------------------------------------------------------------------------------
(fly-main) 
