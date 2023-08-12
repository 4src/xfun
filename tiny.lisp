; vi: set ts=2 sw=2 sts=2 et :
;## Globals

(defvar *settings* '(
  (p       "-p" "dialog asda" 2)
  (dialog "--dialog" "asda"        t)
  (xx     "-p" "asdas"       (2 3))))
;-----------------------------------------------------------------------------------------
(defmacro ? (x) `(cadddr (assoc ',x  *settings*)))

(defmacro aif (test then &optional else) `(let ((it ,test)) (if it ,then ,else)))

(defmacro o (struct slot &rest slots)
   (if slots `(o (slot-value ,struct ',slot) ,@slots)  `(slot-value ,struct ',slot)))  

(defmacro seen (x lst &optional (init 0))
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))
;-----------------------------------------------------------------------------------------
(defstruct sym 
  (at 0) (name " ") (n 0) has (most 0) mode)

(defstruct (num (:constructor %make-num)) 
  (at 0) (name " ") (n 0) (mu 0) (mu2 0) (lo 1E30) (hi -1E30) (heaven 0))

(defun make-num (&key init (at 0) (name " "))
   (%make-num :init init :at at :name name 
              :heaven (if (eq #\- (last-char name)) 0 1)))

(defun make-col (&key init (at 0) (name " "))
  (if (upper-case-p (char name 0))
    (make-num :init init :at at :name name)
    (make-sym :init init :at at :name name)))

(defstruct (cols (:constructor %make-cols))
   x y all names)

(defun make-cols (lst &aux (n -1) (self (%make-cols :names lst)))
  (let ((all (mapcar (lambda (name) (make-col :at (incf n) :name name)) lst)))
    (dolist (col all self)
      (if (not (eq #\X (last-char (o col name))))
        (if (member (last-char (o col name)) '(#\+ #\-))
          (push col y)
          (push col x))))))

(defstruct (row (:constructor %make-row)) cells)
(defun make-row (n) (%make-row :a (/ n 2) :n (/ n 2)))

(defstruct (sheet (:constructor %make-sheet))  rows cols)
(defun make-sheet (src &optional (self ($make-sheet)))
  (adds self src)
  self)

(defmethod adds ((self sheet) (file string))
  (with-file file (lambda (lst) (add self (make-row lst)))))

(defmethod adds ((self sheet) (rows cons))
  (dolist (row rows) (add self row)))

(defmethod add ((self sheet) (row1 row))
  (with-slots (rows cols) self
    (if cols 
      (progn (push row1 rows)
             (mapcar (lambda (col x) (add col x)) cols (o row1 cells)))
      (setf cols (make-cols (row-cells row1))))))

(defmethod add ((self num) x)
  (with-slots (lo hi n mu m2)
    (unless (eq #\? x)
      (incf n)
      (let ((d (- x mu)))
        (incf mu (/ d n))
        (incf m2 (* d (-  x mu)))
        (setf lo (min x lo)
              hi (max x hi))))))

(defmethod add ((self sum) x)
  (with-slots (seen mode most n)
    (unless (eq #\? x)
      (incf n)
      (if (> (incf (seen x has)) most)
        (setf most (cdr (assoc x seen))
              mode x)))))

;-----------------------------------------------------------------------------------------
(defmethod last-char ((s string)) (char s (1- (length s))))
(defmethod last-char ((s symbol)) (last-char (symbol-name s)))

(defun with-file (file fun &optional (filter #'split))
  (with-open-file (s file)
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))

(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  (cond ((string= s1 "?") #\?)
        ((string= s1 "t") t)
        ((string= s1 "nil") nil)
        (t (let ((n (read-from-string s1 nil nil)))
             (if (numberp n) n s1)))))

(defun split (s &optional (sep #\,) (filter #'thing) (here 0))
  (let* ((there (position sep s :start here))
         (word  (funcall filter (subseq s here there))))
    (labels ((tail () (if there (split s sep filter (1+ there)))))
      (if (equal word "") (tail) (cons word (tail))))))

(defun cli (lst)
  (loop for (key flag help b4) in lst collect
        (list key flag help (aif (member flag (args))
                                 (cond ((eq b4 t)    nil)
                                       ((eq b4 nil)  t)
                                       ((stringp b4) (second it))
                                       (t            (read-from-string (second it))))
                                 b4))))

(defun args ()
  #+clisp ext:*args*
  #+sbcl sb-ext:*posix-argv*)

(defun goodbye (&optional (x 0))
  #+clisp (ext:exit x)
  #+sbcl  (sb-ext:exit :code x))

(print(make-row 10))
(print (? p))
(setf *settings* (cli *settings*))
(print (? dialog))
