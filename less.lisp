(defvar *settings* ; car is help text, cdr are the settings
'("
tiny : fun with stuff
(c)2023 Tim Menzies <timm.ieee.org> BSD-2

USAGE :
  OPTIONS sbcl --script tiny.lisp
"
    (BOOTSTRAPS   "number of bootstraps"  256)
    (BOOTCONF     "bootstrap threshold"   .05)
    (CLIFFS       "cliffs delta"          .147)
    (COHEN        "cliffs delta"          .35)
    (EG           "start up actions"      "nothing")
    (FILE         "data file"             "../data/auto93.lisp")
    (HELP         "show help"             nil)
    (P            "distance coefficient"  2)
    (SEED         "random seed"           1234567891)
    ))

(defmacro ? (x) `(caddr (assoc ',x  (cdr *settings*))))

(defmacro o (struct slot &rest slots)
  (if slots `(o (slot-value ,struct ',slot) ,@slots) `(slot-value ,struct ',slot)))

;-------------------------------------------------------
(defun goodbye (&optional (x 0)) #+clisp (ext:exit x) #+sbcl  (sb-ext:exit :code x))

(defun env (x &optional (s (symbol-name x))) ; http://tiny.cc/lispenv
  #+clisp (ext:getenv s) #+scl (cdr (assoc s ext:*environment-list* :test #'string=)))

(defun down-name (x) (string-downcase (symbol-name x)))

(defmethod last-char ((s string)) (char s (1- (length s))))
(defmethod last-char ((s symbol)) (last-char (symbol-name s)))

 (defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  (let ((x (read-from-string s1)))
    (cond ((numberp x) x)
          ((eq x t)    x)
          ((eq x nil)  x)
          (t           s1))))

(defun cli (lst)
  (cons (car lst) (loop :for (key help b4) :in (cdr lst) :collect
                        (list key help (if (env key) (thing (env key)) b4)))))

(defun egs()
  (labels ((eg (x) (equalp "eg-" (subseq (down-name x) 0 (min 3 (length (str x))))))) 
    (loop :for x :being :the symbols :in *package* :if (eg x) :collect x)))

(defun tiny-run (sym &aux (b4 (copy-tree *settings*)))
  (setf *seed* (?  seed))
  (prog1 (funcall sym) (setf *settings* (copy-tree b4))))

(defun main ()
  (labels ((use (x) (member (? eg) `("all" ,(subseq (down-name x) 3)) :test #'string=)))
    (setf  *settings* (cli *settings*))
    (goodbye (loop :for eg :in (egs) :if (use eg) :count (not (tiny-run eg))))))

(defun datas(f) (print f) (with-open-file (s f) (read s)))

;---------------------------------------------------------------
(defun eg-aa() (print 1))
(defun eg-the() (print (cdr *settings*)))

;---------------------------------------------------------------
(setf *settings* (cli *settings*))

(defstruct (cols (:constructor %make-cols)) all x y klass names)

(defmacro (data (:constructor %make-data)) rows cols)

(defun make-cols (lst &aux (n -1) (cols0 (%make-cols :names lst)))
  (with-slots (all x y klass) cols0
    (setf all (mapcar (lambda (s) (make-col :at (incf n) :name s)) lst))
    (dolist (col all cols0)
      (when (not (eq #\X (last-char (o col name))))
        (if (member (last-char (o col name)) '(#\+ #\-))
          (push col (o cols1 y))
          (push col (o cols1 x)))))))

(defun make-data (s &aux (data1 (%make-data :names s)))
  (with-slots (all x y klass) data1
    (setf all (mapcar 
 (dolist (a (datas (? file)) data) (add data1 a)))

(defmethod add ((data1 data) (a cons)) (add data1 (coerce a 'vector)))

(defmethod add ((data1 data) (v vector))
  (with-slots (cols row) data1
    (if  cols
      (push (dolist (col cols v) (add col (elt v (? col at)))) rows)
      (setf cols (make-cols v)))))
