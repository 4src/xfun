# lo.lisp


```lisp
(defun lo (x) (ignore-errors (load x)) (load x))
```


```lisp
(defmacro aif (test this &optional that)
  `(let ((it ,test))            
     (if it ,this ,that)))
```


```lisp
(defun char0 (s) (elt (if (symbolp s) (symbol-name s) s) 0))
(defun charn (s) (elt s (1- (length s))))
```


```lisp
(defstruct row cells)
(defstruct col (at 0) (txt "") (n 0))
(defstruct (sym (:include col)) a)
(defstruct (num (:include col)
                (:constructor %make-num))
           (lo 1E30) (hi -1E30) (mu 0) (m2 0) (w 1))
```


```lisp
(defun make-num (&key (txt "") (at 0))
  (%make-num :at at :txt txt :w (if (eq #\- (charn txt)) 0 1)))
```


```lisp
(defun make-col1 (&key (txt "")  (at 0))
  (if (upper-case-p (char0 txt))
      (make-num :txt txt :at at)
      (make-sym :txt txt :at at)))
```


```lisp
(defstruct (cols (:constructor %make-cols)) x y all klass)
```


```lisp
(defun make-cols (lst &aux (at -1) (self (%make-cols)))
  (with-slots (x y all klass) self
    (dolist (x lst self)
      (let ((col1  (%make-col1 :at (incf at) :txt x))
            (z    (churn x)))
        (push col1 all)
        (unless (eq #\X z)
          (if (eq #\! z) (setf klass col1))
          (if (member z '(#\! #\- #\+)) (push col1 y) (push col1 x)))))))
```


```lisp
(defmethod add ((self cols) (row1 row))
  (with-slots (x y) self
    (dolist (cols1 (list x y))
      (dolist (col1 cols1) (add col1 (elt (row-cells row1) (col-at col1))))))
  row1)
  
(defun slurp(f fun)
  (with-open-file (s f)
    (loop (funcall fun (coerce (or (read s nil) (return)) 'vector)))))
```


```lisp
(defstruct row cells)
(defstruct (data (:constructor %make-data)) rows cols)
```


```lisp
(defmethod add ((self data) (lst cons)) (add self (make-row :cells lst)))
(defmethod add ((self data) (r row))
  (aif (data-cols self)
       (push (add it (row-cells r)) (data-rows self))
       (setf (data-cols self) (make-cols r))))
```


```lisp
(defun make-data (src &aux (self (%make-data)))
  (if (stringp src)
      (slurp src (lambda (x) (add self x)))
      (dolist (one src) (lambda (x) (add self x))))
  self)
```


```lisp
(defun slurp (f)
  (labels ((arrays(lst) (coerce (mapcar #'(lambda(x) (coerce x 'vector)) lst) 'vector)))
    (with-open-file (s f) (arrays (read s)))))
```


```lisp
(defun egs()
  (sort (loop :for x :being :the symbols :in *package* :if (eq #\. (char0 x))  :collect x)
        #'string< :key #'symbol-name))
```


```lisp
(defun .fred () 1)
(defun .andrew () 1)
```


```lisp
(print (elt (slurp "../data/auto93.lisp") 1))
```

