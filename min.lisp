(defstruct bin lo hi ys)
;------------------------------------------------------------------------------
(defstruct sym (n 0) (at 0) (txt " ") (has 0))

(defun new-sym (&optional (at 0) (s ""))
  (make-num :at 0 :txt s))
;------------------------------------------------------------------------------
(defstruct num (n 0) (at 0) (txt " ") (mu 0) (m2 0) (sd 0) (heaven 1))

(defun new-num (&optional (at 0) (s " "))
  (make-num :at 0 :txt s :heaven (if  (end s) #\-) 0 1))
;------------------------------------------------------------------------------
(defstruct cols x y all names klass)

(defun new-cols (lst &aux (n -1) (cols1 (make-cols :name lst)))
  (with-slots (x y all klass) cols1
    (dolist (s lst cols1) 
      (let* ((col (if (upper-case-p (char s 0)) (new-num at s) (new-sym at s))))
        (push col all)
        (unless (eql (end s) #\X)
          (if (eql (end s) #\!) (setf klass col))
          (if (member (end s) '(#\< #\> #\!)) (push col y) (push col x)))))))

(defmethod add ((cols1 cols) lst)
   (mapcar #'(lambda (col x) (add col x)) cols lst))
;------------------------------------------------------------------------------
(defstruct data rows cols fun)

(defun new-data (lst &optional (fun (lambda (&rest _) _)) 
                     &key rank &aux (data1 (make-data :fun fun)))
  (with-slots (rows) data1
    (dolist (row lst) (add data1 row))
    (setf rows (if rank (sort rows :key (lambda (row) (d2d data1 row))) rows))
    data1))

(defmethod add ((data1 data) row)
  (with-slots (rows cols fun) data1
    (cond (cols (funcall (data-fun data1) data1 row)
                (push (add cols row) rows))
          (t    (setf cols (new-cols row))))))

(defmethod d2h ((data1 data) lst)
  (let* ((d 0) 
         (ys (cols-y (data-cols data1))))
    (dolist (col ys (expt (/ d (length ys)) .5)) 
      (with-slots (heaven at) col
        (incf d (expt (abs (- heaven (norm col (elt lst at)))) 2))))))
;------------------------------------------------------------------------------
(defun end(s)
  "Return last char in a string"
  (char s (1- (length s))))

(defun inc (x lst) 
  "Return `lst`'s  slot value for `x` (if missing, initialize x's slot to 0)."    
  (incf (cdr (or (assoc x  lst :test #'equal)
                 (car (setf lst (cons (cons x 0) lst)))))))
            
(defun goodbye (x)
  "Return a value to the operator system."
   #+clisp (ext:exit x) #+sbcl (sb-ext:exit :code x))

(defun args ()
  "Access argv (for both clisp and sbcl"
  #+clisp ext:*args*  #+sbcl sb-ext:*posix-argv*)

(defun str2thing (s)
  "From string extract a number, bool, string, or '? symbol"
  (let* ((s1 (string-trim '(#\Space #\Tab) s))
         (*read-eval* nil)) 
    (read-from-string s1 "")))

(defun cli (options &aux it)
  "Update options from CLU. Booleans need no arg-- we just flip their old value."
  (loop :for (key flag help b4) :in options 
        :collect (list key flag help (if (setf it (member flag (args) :test #'string=))
                                         (cond ((eq b4 t) nil)
                                               ((eq b4 nil) t)
                                                (t (str2thing (second it))))
                                         b4))))
;------------------------------------------------------------------------------
(defun egs ()
  (labels ((eg (x) (equal "eg-" (subseq (format nil "~(~a~)   " (symbol-name x)) 0 3))))
    (loop :for x :being :the symbols :in *package* :if (eg x) :collect x)))

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
