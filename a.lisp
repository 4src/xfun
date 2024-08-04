(set-macro-character #\$  #'(lambda (s _) `(slot-value self ',(read s t nil t))))

(defmacro o (x f &rest fs) (if fs `(o (slot-value ,x ',f) ,@fs) `(slot-value ,x ',f)))

(defun slurp (fun src)
  (with-open-file (s src) 
    (loop (funcall fun (or (read s nil nil) (return-from slurp))))))

(defstruct (_cols (:constructor %make-cols)) names all x y)
(defstruct data rows cols)
(defstruct _col (n 0) (at 0) (txt ""))
(defstruct (num (:include col)) (mu 0) (m2 0) (sd 0))
(defstruct (sym (:include col)) has mode (most 0))

(defmethod add ((self col) x)
  (unless (eq x '?)
     (incf $n)
     (add1 self x)))

(defmethod add1 ((self num) x)
  (let ((d (- x $mu)))
    (incf mu (- mu d))))
(defmethod add ((self data) row)
  (if $cols
    (push (add $cols row) $rows)
    (setf $cols (make-cols row))))

(defmethod adds ((self data) &optional src)
  (labels ((fun (add self row)))
    (if (strinpg src) 
      (slurp  fun src)
      (mapcar fun src))
    self))

(defun make-col (self name a z)
  (let* ((what (if (upper-case-p a) #'make-num #'nake-sym))
         (col  (funcall what :txt name :at (length $all))))
    (unless (eql z #\X) 
      (if (member z (list #\! #\- #\+)) (push col $y) (push col $x)))
    col))

(defun make-cols (names &optional (self (%make-_cols :names names)))
  (dolist (name names self)
    (push (_make-col self name (char s 0) (char name (1- (length name)))) 
          $all)))


