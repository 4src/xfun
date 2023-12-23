#+sbcl
(defun lo (x) (ignore-errors (load x)) (load x))

(defstruct sym (txt 0) (at 0))
(defstruct (num (:constructor %make-num))
           (n 0) (at 0) (txt "") (lo 1E30) (hi -1E30) (mu 0) (m2 0) (w 1))

(defun slurp(f fun)
  (with-open-file (s f)
    (loop (funcall fun (coerce (or (read s nil) (return)) 'vector)))))

(defun make-num (&key (txt "") (at 0))
  (%make-num :at at :txt txt :w (if (eq #\- (charn txt)) 0 1)))

(defun char0 (s) (elt (if (symbolp s) (symbol-name s) s) 0))
(defun charn (x &aux (s x)) (elt s (1- (length s))))

(defun make-col (&key (txt "")  (at 0))
  (if (upper-case-p (char0 txt))
      (make-num :txt txt :at at)
      (make-sym :txt txt :at at)))

(defun make-cols (lst &aux (at -1))
  (mapcar (lambda (txt) (make-col :at (incf at) :txt txt)) lst))

(defun slurp (f)
  (labels ((arrays(lst) (coerce (mapcar #'(lambda(x) (coerce x 'vector)) lst) 'vector)))
    (with-open-file (s f) (arrays (read s)))))

(defun egs()
  (sort (loop :for x :being :the symbols :in *package* :if (eq #\. (char0 x))  :collect x)
        #'string< :key #'symbol-name))

(defun .fred () 1)
(defun .andrew () 1)

(print (elt (slurp "../data/auto93.lisp") 1))
