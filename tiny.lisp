; vi: set ts=2 sw=2 sts=2 et :
;## Globals
(defvar *help* "
tiny : fun with stuff
(c)2023 Tim Menzies <timm.ieee.org> BSD-2

USAGE:
  sbcl --script tiny.lisp")

(defvar *settings* '(
  (p       "-p"      "dialog asda" 2)
  (dialog "--dialog" "asda"        t)
  (seed   "--seed"   "randseed"    1234567891)
  (help   "-h"       "shpw help"   nil)
  (eg     "-e"       "start up example" "nothing")
  (xx     "-p"       "asdas"       (2 3))))
;-----------------------------------------------------------------------------------------
(defmacro ? (x) `(cadddr (assoc ',x  *settings*)))

(defmacro aif (test then &optional else) 
  `(let ((it ,test)) 
     (if it ,then ,else)))

(defmacro o (struct slot &rest slots)
  (if slots 
    `(o (slot-value ,struct ',slot) ,@slots)  
    `(slot-value ,struct ',slot)))  

(defmacro seen (x lst &optional (init 0))
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))
;-----------------------------------------------------------------------------------------
(defun args ()
  #+clisp ext:*args*
  #+sbcl sb-ext:*posix-argv*)

(defun goodbye (&optional (x 0))
  #+clisp (ext:exit x)
  #+sbcl  (sb-ext:exit :code x))

(defvar *seed* 10013)
(defun rand (&optional (n 1))
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 1) &aux (base 10000000000.0))
  (floor (* n (/ (rand base) base))))

(defmethod last-char ((s string)) (char s (1- (length s))))
(defmethod last-char ((s symbol)) (last-char (symbol-name s)))

(defun cli (lst)
  (loop for (key flag help b4) in lst collect
        (list key flag help (aif (member flag (args) :test #'string=)
                                 (cond ((eq b4 t) nil)
                                       ((eq b4 nil) t)
                                       ((stringp b4) (second it))
                                       (t  (read-from-string (second it))))
                                 b4))))

(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  (if (string= s1 "?") #\?
    (let ((x (read-from-string s1)))
      (print (list 'things x (type-of x) s1))
      (cond ((numberp x) x)
            ((eq x t)    x)
            ((eq x nil)  x)
            (t           s1)))))

(defun split (s &optional (sep #\,) (filter #'thing) (here 0))
  (let* ((there (position sep s :start here))
         (word  (funcall filter (subseq s here there))))
    (labels ((tail () (if there (split s sep filter (1+ there)))))
      (if (equal word "") (tail) (cons word (tail))))))

(defun with-file (file fun &optional (filter #'split))
  (with-open-file (s file)
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))
;-----------------------------------------------------------------------------------------
(defstruct sym 
  (at 0) (name " ") (n 0) has (most 0) mode)

(defstruct (num (:constructor %make-num)) 
  (at 0) (name " ") (n 0) (mu 0) (mu2 0) (lo 1E30) (hi -1E30) (heaven 0))

(defun make-num (&key (at 0) (name " "))
    (%make-num :at at :name name 
                          :heaven (if (eq #\- (last-char name)) 0 1)))

(defun make-col (&key (at 0) (name " "))
  (if (upper-case-p (char name 0))
    (make-num  :at at :name name)
    (make-sym  :at at :name name)))

(defstruct (cols (:constructor %make-cols))
   x y all names)

(defun make-cols (lst &aux (n -1) (self (%make-cols :names lst)))
  (let ((all (mapcar (lambda (name) (make-col :at (incf n) :name name)) lst)))
    (dolist (col all self)
      (if (not (eq #\X (last-char (o col name))))
        (if (member (last-char (o col name)) '(#\+ #\-))
          (push col (cols-y self))
          (push col (cols-x self)))))))

(defstruct row cells)

(defstruct (sheet (:constructor %make-sheet))  rows cols)
(defun make-sheet (src &optional (self (%make-sheet)))
  (adds self src)
  self)

(defmethod add ((self sheet) (row1 row))
  (with-slots (rows cols) self
    (if cols 
      (progn (push row1 rows)
             (mapcar (lambda (col x) (add col x)) cols (o row1 cells)))
      (setf cols (make-cols (row-cells row1))))))

(defmethod add ((self num) x)
  (with-slots (lo hi n mu m2) self
    (unless (eq #\? x)
      (incf n)
      (let ((d (- x mu)))
        (incf mu (/ d n))
        (incf m2 (* d (-  x mu)))
        (setf lo (min x lo)
              hi (max x hi))))))

(defmethod add ((self sym) x)
  (with-slots (seen mode most n has) self
    (unless (eq #\? x)
      (incf n)
      (if (> (incf (seen x has)) most)
        (setf most (cdr (assoc x seen))
              mode x)))))

(defmethod adds ((self sheet) (file string))
  (with-file file (lambda (lst) (add self (make-row :cells lst)))))

(defmethod adds ((self sheet) (rows cons))
  (dolist (row rows) (add self row)))

(defun eg_fred () 
  "print something"
  (print 1))

(defun eg_jane () 
  "print something"
  1)

(defun tiny (&optional (pre "EG_"))
  "assumes there is a *settings* and *help*"
  (labels 
    ((str  (sym) (symbol-name sym))
     (eg   (x)   (equalp pre (subseq (str x) 0 (min (length pre) (length (str x)))))) 
     (egs  ()    (loop for x being the symbols in *package* if (eg x) collect x))
     (use  (x)   (member (? eg) `("all" ,(string-downcase (subseq (str x) (length pre)))) 
                         :test #'string=))
     (uses (lst) (loop for x in lst if (use x) collect x))
     (run  (sym) (let ((b4 (copy-tree *settings*)))
                   (setf *seed* (? seed))
                   (let ((status (funcall sym)))
                     (unless status (format t "FAIL: ❌ ~a~%" sym))
                     (setf *settings* (copy-tree b4))
                     status)))
     (show ()    (format t "~a~%~%" *help*)
                 (loop for (_ s1 s2 __) in *settings* do (format t "  ~10a  ~a~%" s1 s2))
                 (format t "~%OPTIONS:~%")
                 (loop for x in (egs) do
                     (format t "  -e ~(~8a~) ~a~%" (subseq (str x) (length pre)) 
                        (documentation x 'function)))))
    (if (? help)
      (show)
      (loop for eg in (uses (egs)) sum (if (run eg) 0 1)))))

(setf *settings* (cli *settings*))
(goodbye (tiny))
