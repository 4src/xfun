; vim : set ts=3 sw=3 sts=3 et :
;## Globals
(defvar *help* "
tiny : fun with stuff
(c)2023 Tim Menzies <timm.ieee.org> BSD-2

USAGE:
  sbcl --script tiny.lisp")

(defvar *settings* 
  '(
    (cliffs "-c"  "cliffs delta"      0.147)
    eg      "-e"  "start up example"  "nothing")
    (file   "-f"  "data file"         "../data/auto93.csv")
    (help   "-h"  "shpw help"         nil)
    (p      "-p"  "dialog asda"       2)
    (seed   "-s"  "random seed"       1234567891)
    ))
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

(defun round2 (number &optional (digits 2))
  "round to `digits` number of decimal places"
  (let* ((div (expt 10 digits))
         (tmp (/ (round (* number div)) div)))
    (if (zerop digits) (floor tmp) (float tmp))))

(defvar *seed* 10013)
(defun rand (&optional (n 1))
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 1) &aux (base 10000000000.0))
  (floor (* n (/ (rand base) base))))

(defmethod shuffle ((a cons)) 
  (coerce (shuffle (coerce a 'simple-vector)) 'cons))
(defmethod shuffle ((a array)) 
  (loop for i from (length a) downto 2 do (rotatef (elt a (rint i)) (elt a (1- i))))
  a)

(defun time-it (fun &optional (repeats 1))
  (let ((t0 (get-internal-real-time)))
    (dotimes (_ repeats) (funcall fun))
    (float (/ (-(get-internal-real-time) t0) repeats))))

(defun normal (&optional (mu 0) (sd 1)) 
  (+ mu (* sd (sqrt (* -2 (log (rand)))) (cos (* 2 pi (rand))))))

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
      (cond ((numberp x) x)
            ((eq x t)    x)
            ((eq x nil)  x)
            (t           s1)))))

(defun split (s &optional (sep #\,) (filter #'thing) (here 0))
  (let* ((there (position sep s :start here))
         (word  (funcall filter (subseq s here there))))
    (labels ((tail () (if there (split s sep filter (1+ there)))))
      (if (equal word "") (tail) (cons word (tail))))))

(defun with-lines (file fun &optional (filter #'split))
  (with-open-file (s file)
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))

(defun cliffs-delta (xs ys &aux (n 0) (lt 0) (gt 0))
  (let ((n1 (length xs))
        (n2 (length ys)))
    (cond ((> n1 (* 10 n2)) (cliffs-delta (subseq (shuffle xs) 0 (* 10 n2)) ys))
          ((> n2 (* 10 n1)) (cliffs-delta xs (subseq (shuffle ys) 0 (* 10 n1))))
          (t (dolist (x xs (> (/ (abs (- gt lt)) n) (? cliffs)))
               (dolist (y ys)
                 (incf n)
                 (if (> x y) (incf gt))
                 (if (< x y) (incf lt))))))))
;-----------------------------------------------------------------------------------------
(defstruct sym 
  (at 0) (name " ") (n 0) has (most 0) mode)

(defmethod mid ((s sym)) (sym-mode s))
(defmethod div ((s sym))
  (with-slots (has n) s
    (* -1  (loop for (_ . v) in has sum  (* (/ v n)  (log (/ v n) 2))))))

(defstruct (num (:constructor %make-num)) 
  (at 0) (name " ") (n 0) (mu 0) (m2 0) (lo 1E30) (hi -1E30) (heaven 0))

(defun make-num (&key (at 0) (name " "))
  (%make-num :at at :name name :heaven (if (eq #\- (last-char name)) 0 1)))

(defmethod mid ((n num)) (num-mu n))
(defmethod div ((n num)) (sqrt (/ (num-m2 n) (- (num-n n) 1))))

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
  (with-slots (mode most n has) self
    (unless (eq #\? x)
      (incf n)
      (if (> (incf (seen x has)) most)
        (setf most (cdr (assoc x has))
              mode x)))))

(defmethod adds ((self sheet) (file string))
  (with-lines file (lambda (lst) (add self (make-row :cells lst)))))

(defmethod adds (self (lst cons))
  (dolist (item lst self) (add self item)))
;-----------------------------------------------------------------------------------------
(defun eg-fail()
  "can the test engine handle a fail?"
   nil)

(defun eg-set () 
  "are the settings ok?"
  (format t "~{~a~%~}" *settings*)
  (dolist (x '(help seed file eg) t)
    (or (cdr (assoc x *settings*)) 
        (return-from eg-set (format t "missing in *settings* : ~a~%" x)))))

(defun eg-rand () 
  "if seed reset, then same psuedoi-randoms?"
  (let (a b)
    (setf *seed* 1) (setf a (sort (loop repeat 10 collect (rint 100)) #'<))
    (setf *seed* 1) (setf b (sort (loop repeat 10 collect (rint 100)) #'<))
    (equal a b)))

(defun eg-file (&aux (n 0))
  "can count cells in a csv file?"
  (with-lines (? file) (lambda (a) (incf n (length a))))
  (= n 3192))

(defun eg-sym ()
  "can compute entropy?"
  (< 1.378 (div (adds (make-sym) '(a a a a b b c))) 1.388))
 
(defun eg-num()
  "can compute mu and standard deviation?"
  (let ((num (adds (make-num) (loop repeat 10000 collect (normal 10 2)))))
    (and (< 9.95 (mid num) 10.05) (< 1.95 (div num) 2.05))))

(defun eg-shuffle ()
  "can numbers be shuffled?"
  (let ((nums (loop for x upto 20 collect x)))
    (equal nums  (sort (shuffle (copy-tree nums)) #'<))))

;-----------------------------------------------------------------------------------------
(defun tiny (&optional (pre "eg-") (w 3))
  (labels 
    ((str  (sym) (string-downcase (symbol-name sym)))
     (eg   (x)   (equalp pre (subseq (str x) 0 (min w (length (str x)))))) 
     (egs  ()    (loop for x being the symbols in *package* if (eg x) collect x))
     (use  (x)   (member (? eg) `("all" ,(subseq (str x) w)) :test #'string=))
     (uses (lst) (loop for x in lst if (use x) collect x))
     (run  (sym &aux (b4 (copy-tree *settings*)))
           (setf *seed* (? seed))
           (prog1
             (or (funcall sym) 
                 (format t "~&❌ FAIL: ~a~%" sym))
             (setf *settings* (copy-tree b4))))
     (show-help ()    
           (format t "~a~%~%" *help*)
           (loop for (_ s1 s2 __) in *settings* do (format t "  ~10a  ~a~%" s1 s2))
           (format t "~%OPTIONS:~%")
           (loop for x in (egs) do
                 (format t "  -e ~8a ~a~%" 
                         (subseq (str x) w) (documentation x 'function)))))
    (setf *settings* (cli *settings*))
    (if (? help)
      (show-help)
      (goodbye (loop for eg in (uses (egs)) sum (if (run eg) 0 1))))))

(tiny)
