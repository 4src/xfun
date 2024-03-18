;------------------------------------------------------------------------------
(defmacro o (struct slot &rest slots) 
  "Nested slot access. (o obj s1 s2 s3) return s3's slot from s2's slot from s1 of obj."
  (if slots `(o (slot-value ,struct ',slot) ,@slots) 
    `(slot-value ,struct ',slot)))   

(defmacro has (x lst) 
  "Return `lst`'s  slot value for `x` (if missing, initialize x's slot to 0)."    
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x 0) ,lst))))))

(defmacro thing (it &rest has) 
  "Add constructors to defstructs. And a method to return all the slots."
  (labels ((make (x) (intern (format nil "%MAKE-~a" x))) 
           (name (x) (if (consp x) (car x) x))) 
    `(progn (defstruct (,it (:constructor ,(make it))) ,@has)
            (defmethod slots-of ((_ ,it)) ',(mapcar #'name has)))))

(defmacro things (&rest defstructs)
  "Apply `thing` to a list of defstructs." 
  `(progn ,@(loop for (defstruct . slots) in defstructs collect `(thing ,@slots))))
;------------------------------------------------------------------------------
(things
  (defstruct bin lo hi ys)
  (defstruct data rows cols fun)
  (defstruct cols x y all names klass)
  (defstruct sym (n 0) (at 0) (txt " ") (has 0))
  (defstruct num (n 0) (at 0) (txt " ") (mu 0) (m2 0) (sd 0) (heaven 1)))
;------------------------------------------------------------------------------
(defun make-sym (&optional (at 0) (s ""))
  ($make-num1 :at 0 :txt s))

(defun make-num (&optional (at 0) (s " "))
  (%make-num :at 0 :txt s :heaven (if  (end s) #\-) 0 -1))
  
(defun make-cols (lst &aux (n -1) (cols1 (%make-cols :name lst)))
  (with-slots (x y all klass) cols1
    (dolist (s lst cols1) 
      (let* ((col (if (upper-case-p (char s 0)) (name-num at s) (make-sym at s))))
        (push col all)
        (unless (eql (end s) #\X)
          (if (eql (end s) #\!) (setf klass col))
          (if (member (end s) '(#\< #\> #\!)) (push col y) (push col x)))))))

(defmethod add ((cols1 cols) lst)
   (mapcar #'(lambda (col x) (add col x)) cols lst))

(defun noop (&rest _) _)

(defun make-data (lst &optional (fun noop)  &key rank &aux (data1 (%make-data :fun fun)))
  (with-slots (rows) data1
    (dolist (row lst) (add data1 row))
    (setf rows (if rank (sort rows :key (lambda (row) (d2d data1 row))) rows))
    data1))

(defmethod add ((data1 data) row)
  (with-slots (rows cols fun) data1
    (cond (cols (funcall (data-fun data1) data1 row)
                (push (add cols row) rows))
          (t    (setf cols (make-cols row))))))

(defmethod d2h ((data1 data) lst)
  (let* ((d 0) (ys (o data1 cols y)))
    (dolist (col ys (expt (/ d (length ys)) .5)) 
      (with-slots (heaven at) col
        (incf d (expt (abs (- heaven (norm col (elt lst at)))) 2))))))
;------------------------------------------------------------------------------
(defun chend (s)
  "Return last item in a string."
  (char s (1- (length s))))  

(defun goodbye (x)
  "Return a value to the operator system."
   #+clisp (ext:exit x) #+sbcl (sb-ext:exit :code x))

(defun args ()
  "Access argv (for both clisp and sbcl"
  #+clisp ext:*args*  #+sbcl sb-ext:*posix-argv*)

(defun str2thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  "From string extract a number, bool, string, or '? symbol"
  (let ((it (let ((*read-eval* nil)) (read-from-string s1 ""))))
    (cond ((numberp it)     it)
          ((eq it t)        t)
          ((eq it nil)      nil)
          ((string= it "?") '?)
          (t                s1))))
  
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
;------------------------------------------------------------------------------
     