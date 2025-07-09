#|
defstruct about
  (what "ezr.lisp") (who "tim menzies") (why "clustering on x values") 
  (when 2024) (copyright "bsd-2")) 
|#
(defpackage :my (:use :cl) (:shadow :map))
(in-package :my)

(defvar *seed* 123456890)
(defvar *train* "data/atho93.lisp")

(set-macro-character #\$ #'(lambda (s _) `(slot-value i ',(read s))))

(defmacro o (s f &rest fs)
  (if fs `(o (slot-value ,s ',f) ,@fs) `(slot-value ,s ',f)))

(set-macro-character #\$ #'(lambda (s _) (slot-value i ',(read s))))

(defun mapcsv (fun file)
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (or (read s nil nil) (return))))))

(defstruct col (n 0) (at 0) (txt ""))
(defstruct (sym (:include col)) has)
(defstruct (num (:include col)) (mu 0) (m2 0) (sd 0)  (hi -1E32) (lo 1E32))
(defstruct cols all x y klass)
(defstruct data name header rows)

(defun makedata (src &aux (i (make-data)))
  (labels ((_add (row) (add i row)))
    (if (stringp src) (mapcsv #'_add src) (mapcar #'_add src))
    i))

(defmethod add ((i data) row)
  (if $cols
   (setf $cols (makecols row))
   (push $rows (mapcar (lambda (col) (add col (elt row (col-at col))))))

   
