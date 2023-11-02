(defvar *settings* ; car is help text, cdr are the settings
'("
tiny : fun with stuff
(c)2023 Tim Menzies <timm.ieee.org> BSD-2

USAGE :
  OPTIONS sbcl --script tiny.lisp
"
    (bootstraps   "number of bootstraps"  256)
    (bootConf     "bootstrap threshold"   .05)
    (cliffs       "cliffs delta"          .147)
    (cohen        "cliffs delta"          .35)
    (eg           "start up actions"      "nothing")
    (file         "data file"             "../data/auto93.lisp")
    (help         "show help"             nil)
    (p            "distance coefficient"  2)
    (seed         "random seed"           1234567891)
    ))

(defmacro ? (x) `(caddr (assoc ',x  (cdr *settings*))))

;-------------------------------------------------------
(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  (let ((x (read-from-string s1)))
    (cond ((numberp x) x)
          ((eq x t)    x)
          ((eq x nil)  x)
          (t           s1))))

(defun cli (lst)
  (cons (car lst) 
        (loop :for (key help b4) :in (cdr lst) :collect
          (list key help (if (env key) (thing (env key)) b4)))))

(defun tiny-run (sym &aux (b4 (copy-tree *settings*)))
  (setf *seed* (?  seed))
  (prog1 (funcall sym) 
    (setf *settings* (copy-tree b4))))

(defun egs()
  (labels
    ((str  (sym) (string-downcase (symbol-name sym)))
     (eg   (x)   (equalp "eg-" (subseq (str x) 0 (min 3 (length (str x))))))) 
    (loop :for x :being :the symbols :in *package* :if (eg x) :collect x)))

(defun datas(f) (print f) (with-open-file (s f) (read s)))

(defun env (x &optional (s (symbol-name x)))
  "https://stackoverflow.com/questions/44236376/how-do-i-get-the-list-of-all-environment-variables-available-in-a-lisp-process"
  #+clisp (ext:getenv s)
  #+scl (cdr (assoc s ext:*environment-list* :test #'string=)))

;---------------------------------------------------------------
(defun eg-aa() (print 1))
(defun eg-the() (print (cdr *settings*)))

;---------------------------------------------------------------
(setf *settings* (cli *settings*))

(defmacro data rows cols)

(defun data! (s &aux (data1 (make-data))))
 (dolist (a (datas (? file)) data) (add data1 a)))

(defmethod add ((data1 data) (a cons)) (add data1 (coerce a 'vector)))

(defmethod add ((data1 data) (v vector))
  (with-slots (cols row) data1
    (if  cols
      (dolist (col cols) (add col (elt v (? col at))))
      (setf cols (cols! v)))))

