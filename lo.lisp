(defstruct sym (txt 0) (at 0))
(defstruct (num (:constructor %make-num))
           (n 0) (at 0) (txt "") (lo 1E30) (hi -1E30) (mu 0) (m2 0) (w 1))
 
(defun make-num (&key (txt "") (at 0))
  (%make-num :n n :at at :txt txt :w (if (eq #\- (charn s)) 0 1)))

(defun arrays(lst) (coerce (mapcar #'(lambda(x) (coerce x 'vector)) lst) 'vector))

(defun slurp (f) (with-open-file (s f) (arrays (read s))))

(defmethod char0 (x) (elt x 0))
(defmethod charn (x &aux (s x)) (elt s (1- (length s))))

(defun make-col (&key (txt "")  (at 0))
  (funcall (if (upper-case-p (char0 txt)) #'make-name #'make-sym) :txt txt :at at))

(defun make-cols (lst &aux (at -1))
  (mapcar (lambda (txt) (make-col (incf at))) lst))

(defun egs()
  (sort (loop :for x :being :the symbols :in *package* :if (eq #\. (char0 x)) :collect x)
        #'string<))

(defun .fred () 1)

(print (elt (arrays (slurp "../data/auto93.lisp")) 1))
