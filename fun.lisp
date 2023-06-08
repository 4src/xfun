; vi: set ts=2 sw=2 sts=2 et :
(defvar *settings* '("
fun.lisp: LISP code for multi-objective semi-supervised explanations
(c) 2023, Tim Menzies"
  (bins "-b" "number of bins"      16)
  (file "-f" "where to read data"  "../data/auto93.csv")
  (go   "-g" "start up action"     help)))

(defmacro of (key)
  "Access setting value.s"
  `(fourth (assoc ',key (cdr *settings*))))

(defstruct (data (:constructor %make-data)) rows cols)
(defstruct (cols (:constructor %make-cols)) names all x y klass)

(defstruct sym 
  "summarizes a stream of symbols"
  (at 0) (txt "") (n 0) has (most 0) mode)

(defstruct (num  (:constructor %make-num))
  "summarizes a stream of numbers"
  (at 0) (txt "") (n 0) (w 1) ; w=1,-1 means "maximize", "minimize"
  (hi most-negative-fixnum) 
  (lo most-positive-fixnum)
  (mu 0) (m2 0))

(defun make-num (&key (at 0) (txt "") &aux (self (%make-num :txt txt :at at)))
  (if (eql #\- (last-char txt))
    (setf (num-w self)  -1))
  self)

(defun make-cols (names &aux (self (%make-cols: names names)))
  (with-slots (all x y klass) self
    (loop :for at :from 0 :and s :in names :do
          (let ((z    (last-char s))
                (what (if (upper-case-p (char s 0) #'make-num #'make-sym)))
                (col  (funcall what :at at :txt s)))
            (push col all) 
            (unless (eql z  #\X)
              (if (eql z #\!) (setf klass col))
              (if (member z '(#\+ #\- #!)) (push col y) (push col x))))))
  self)

(defun make-data (&keys cols  rows &aux (self (%make-data)))
  (setf (data-cols self) (make-cols cols)
        (data-rows self) rows)
  (dolist (row rows self)
    (mapc #'add (cols-all (data-cols self)) row)))

(defmethod add ((self sym) x)
  "update frequency counts (in `has`) and `most` and `mode`"
  (with-slots (has n mode most) self
   (unless (eql x #\?)
     (incf n)
     (incf (freq x has))
     (if (> (freq x has) most) (setf most (freq x has) mode x)))))

(defmethod add ((self num) x ) ;;; Add one thing, updating 'lo,hi'
  "updates `lo`, `hi`, `mu`, `sd`"
  (with-slots (n lo hi mu m2) self
    (unless (eq x #\?)
      (incf n)
      (let ((d (- x mu)))
        (incf mu (/ d n))
        (incf m2 (* d (- x mu)))
        (setf lo (min x lo)
              hi (max x hi))))))

; ---------------------------------------------------------------
(defun last-char(s)
  (if (> (length s) 0) (char s (1- (length s)))))

(defun updates (settings)
  "Replace setting values, if a command-line flag asks you."
  (dolist (four (cdr settings) settings)
    (let* ((it (member (second four) (argv) :test #'equal)))
      (if it
        (setf (fourth four)  (update (fourth four) (second it)))))))

(defun update (current command-line-arg)
  "For booleans, no need of a command-line-arg,  just flip the value.
  Else try to read a number, and if that flag, just return as a string"
  (cond ((eql current  t)   nil)
        ((eql current  nil) t)
        (t (let ((n (read-from-string command-line-arg nil nil))) 
             (if (numberp n) n command-line-arg)))))

(defun argv () 
  "Accessing command-line flags"
  #+clisp ext:*args*  
  #+sbcl sb-ext:*posix-argv*)

(defvar *auto93*  
  (data
    :cols  '("Cyndrs" "Vol" "Hpx" "Lbs-" "Acc+" "Model" "origin" "Mpg+")
    :rows  '((8 304.0 193 4732 18.5 70 1 10)
            (8 360 215 4615 14 70 1 10)
            (8 307 200 4376 15 70 1 10)
            (8 318 210 4382 13.5 70 1 10)
            (8 429 208 4633 11 72 1 10)
            (8 400 150 4997 14 73 1 10)
            (8 350 180 3664 11 73 1 10)
            (8 383 180 4955 11.5 71 1 10)
            (8 350 160 4456 13.5 72 1 10)
            (8 429 198 4952 11.5 73 1 10)
            (8 455 225 4951 11 73 1 10)
            (8 400 167 4906 12.5 73 1 10)
            (8 350 180 4499 12.5 73 1 10)))))
