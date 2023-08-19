(defstruct settings (file "../data/auto93.csv"))
(defvar *settings* (make-settings))

(defmacro ? (x) `(slot-value *settings* ',x))

(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  (let ((x (read-from-string s1)))
    (cond ((numberp x) x)
          ((eq x t)    x)
          ((eq x nil)  x)
          ((eq x '?)   x)
          (t           s1))))

(defun split (s &optional (sep #\,)  (here 0))
  (let* ((there (position sep s :start here))
         (word  (subseq s here there)))
    (cons word (if there (split s sep (1+ there))))))

(defun with-lines (file fun)
  (with-open-file (s (or file  *standard-input*))
    (loop (funcall fun (or (read-line s nil) (return))))))

(defun elast (s) (elt s (1- (length s))))

(defun ignore? (s) (eql #\X (elast s)))
(defun numcol? (s) (upper-case? #\X (elt s 0)))
(defun goal?   (s) (member (elast s) '(#\+ #\-)))
(defun less?   (s) (eql (elast s) #\-))

(defstruct col (at 0) (name ""))
(defstruct (sym (:include col)))
(defstruct (num (:include col)) (lo 1E30) (hi -1E30) (heaven 0))

(defun sym! (at name) (make-sym :at at :name name))
(defun num! (at name) (make-num :at at :name name :heaven (if (less? name) 0 1)))

(defun col! (at name)
  (funcall (if (numcol? name) #'num! #'sym!)  at name))

(defstruct cols names all x y)
(defun cols! (lst &aux (at -1))
  (let ((i (make-cols :names lst :all (mapcar (lambda (s) (col! (incf at) s)) lst))))
    (dolist (col (reverse (cols-all i)) i)
      (unless (ignore? (col-name col))
        (if (goal? (col-name col))
          (push col (cols-y i))
          (push col (cols-x i)))))))

(defmethod add (i  x ) x)
(defmethod add ((i num) (x number))
  (setf (num-lo i) (min (num-lo i) x))
  (setf (num-hi i) (max (num-hi i) x)))
