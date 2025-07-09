(defstruct about
  (what "ezr.lisp") (who "tim menzies") (why "clustering on x values") 
  (when 2024) (copyright "bsd-2")) 

(defstruct config (seed  10013) (train "data/auto93.lisp") (about (make-about)))

(defvar *config* (make-config))

(defstruct data name header rows)
(defstruct col (n 0) (at 0) (txt ""))
(defstruct (sym (:include col)) has)
(defstruct (num (:include col)) (mu 0) (m2 0) (sd 0)  (hi -1E32) (lo 1E32))
(set-macro-character #\$  #'(lambda (s _) `(slot-value self ',(read s t nil t))))

(defmacro o (s f &rest fs) (if fs `(o (slot-value ,s ',f) ,@fs) `(slot-value ,s ',f)))
(defmacro is (&rest lst) (o *config* ,@lst))
(defmacro ? (&rest fs) `(o *config* ,@fs))

(defun reads (&key file fun)
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (or (read s nil nil) (return))))))

(defmethod from-file ((self data) file)
  (reads :file file :fun #'(lambda (r) (push r (slot-value self 'rows))))
  self):q
:

(defmethod add ((col self) x)
  (unless (eq x '?) 
    (incf $n)
    (setf x (add1 self x)))
  x)

(defmethod add1 ((self num) (x string))
  (add1 self (read-from-string x nil nil)))

(defmethod add1 ((self num) (x number))
  (let ((d (- x $mu)))
    (incf $mu (/ d $n))
    (incf $m2 (* d (-  x $mu)))
    (setf $lo (min x $lo)
          $hi (max x $hi))))

(defmethod add1 ((self sym) (x atom))
  (incf (cdr (or (assoc x $has :test #'equal) 
                 (car (setf $has (cons (cons x 0) $has)))))))


(defun egs ()
  (labels ((reads () (from-file (make-data) (? train))))
    (reads)
    ))

(print (egs))
