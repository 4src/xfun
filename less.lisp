(defvar +about+ "
LESS.lisp : less is more
(c)2023 Tim Menzies <timm.ieee.org> BSD-2

USAGE :
  OPTIONS sbcl --script tiny.lisp"

(defvar *opts* nil)
(defun opt (flag key help default)
  (push (cons key flag value ) *opts*)
  (format nil "  ~4a [~20a] ~a~%" key help default)))

(defconstant +help+ 
  (with-output-to-string (s)
    (format s "~aOPTIONS:~%~(~a)~%" +about+ (apply #'opt
            '(("-b" BOOTSTRAPS "number of bootstraps"           256)
              ("-B" BOOTCONF   "bootstrap threshoa"             .05) 
              ("-d" COHEN      "Cohen delta"                    .35)
              ("-e" EG         "start up actions nothing" "nothing")
              ("-f" FILE       "data file"    "../data/auto93.lisp")
              ("-h" HELP       "show help"                      nil)
              ("-p" P          "distance coeffecient"             2)
              ("-s" SEED       "random seed"                   7001))))))

(defmacro ? (key) `(third (assoc ',key *opts*)))

(defmacro aif (test then &optional else) 
  `(let ((it ,test)) (if it ,then ,else)))

(defmacro aif (test then &optional else) 
  `(let ((it ,test)) 
     (if it ,then ,else)))

(defmacro o (struct f &rest fs)
  (if fs `(o (slot-value ,struct ',f) ,@fs) `(f-value ,struct ',f)))  

(defmacro seen (x lst &optional (init 0))
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

(defun args ()
  #+clisp ext:*args*
  #+sbcl sb-ext:*posix-argv*)

(defun goodbye (&optional (x 0))
  #+clisp (ext:exit x)
  #+sbcl  (sb-ext:exit :code x))

(defun trim (s)  (string-trim '(#\Space #\Tab) s))

(defun thing (s &aux (s1 (trim s)))
  (let* ((*read-eval* nil)
         (x (read-from-string s1)))
    (cond ((numberp x) x)
          ((eq x t)    t)
          ((eq x nil)  nil)
          (t           s1))))

(defun split (s &optional (here 0))
  (let* ((there (position #\, s :start here))
         (cons (thing (subseq s here there)
                      (if there (split s (1+ there))))))))

(defun cli (lst)
  (loop :for (flag key b4) :in lst :collect
    (list flag key (aif (member flag (args) :test #'string=)
                     (cond ((eq b4 t) nil)
                           ((eq b4 nil) t)
                           (t (eval (second it))))
                     b4))))

(print (lines +settings+))

;
; (defmacro o (struct slot &rest slots)
;   (if slots `(o (slot-value ,struct ',slot) ,@slots) `(slot-value ,struct ',slot)))
;
; ;-------------------------------------------------------
; (defun goodbye (&optional (x 0)) #+clisp (ext:exit x) #+sbcl  (sb-ext:exit :code x))
;
; (defun env (x &optional (s (symbol-name x))) ; http://tiny.cc/lispenv
;   #+clisp (ext:getenv s) #+scl (cdr (assoc s ext:*environment-list* :test #'string=)))
;
; (defun down-name (x) (string-downcase (symbol-name x)))
;
; (defmethod last-char ((s string)) (char s (1- (length s))))
; (defmethod last-char ((s symbol)) (last-char (symbol-name s)))
;
;  (defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
;   (let ((x (read-from-string s1)))
;     (cond ((numberp x) x)
;           ((eq x t)    x)
;           ((eq x nil)  x)
;           (t           s1))))
;
; (defun cli (lst)
;   (cons (car lst) (loop :for (key help b4) :in (cdr lst) :collect
;                         (list key help (if (env key) (thing (env key)) b4)))))
;
; (defun egs()
;   (labels ((eg (x) (equalp "eg-" (subseq (down-name x) 0 (min 3 (length (str x))))))) 
;     (loop :for x :being :the symbols :in *package* :if (eg x) :collect x)))
;
; (defun tiny-run (sym &aux (b4 (copy-tree *settings*)))
;   (setf *seed* (?  seed))
;   (prog1 (funcall sym) (setf *settings* (copy-tree b4))))
;
; (defun main ()
;   (labels ((use (x) (member (? eg) `("all" ,(subseq (down-name x) 3)) :test #'string=)))
;     (setf  *settings* (cli *settings*))
;     (goodbye (loop :for eg :in (egs) :if (use eg) :count (not (tiny-run eg))))))
;
; (defun datas(f) (print f) (with-open-file (s f) (read s)))
;
; ;---------------------------------------------------------------
; (defun eg-aa() (print 1))
; (defun eg-the() (print (cdr *settings*)))
;
; (defun split (s &optional (here 0))
;   (if s
;     (let* ((there (position #\, s :start here)))
;       (cons (thing (subseq s here there))
;             (if there (split s (1+ there)))))))
;
; (defun lines (str) 
;   (with-input-from-string (s str) 
;     (loop 
;       :while   (setf x (split (read-line s nil))) 
;       :if      (eql "--" (subseq (first x) 0 (min (length (first x)) 2)))
;       :collect (list (first x) (second x) (car (last x))))))
;
; (print (lines "aa asdsa ,aab,ccc,dee,1,222,3
;  asd,aa
;  asdasasd,aaa"))
; ;
; ;---------------------------------------------------------------
; ; (setf *settings* (cli *settings*))
; ;
; ; (defstruct (cols (:constructor %make-cols)) all x y klass names)
; ;
; ; (defmacro (data (:constructor %make-data)) rows cols)
; ;
; ; (defun make-cols (lst &aux (n -1) (cols0 (%make-cols :names lst)))
; ;   (with-slots (all x y klass) cols0
; ;     (setf all (mapcar (lambda (s) (make-col :at (incf n) :name s)) lst))
; ;     (dolist (col all cols0)
; ;       (when (not (eq #\X (last-char (o col name))))
; ;         (if (member (last-char (o col name)) '(#\+ #\-))
; ;           (push col (o cols1 y))
; ;           (push col (o cols1 x)))))))
; ;
; ; (defun make-data (s &aux (data1 (%make-data :names s)))
; ;   (with-slots (all x y klass) data1
; ;     (setf all (mapcar 
; ;  (dolist (a (datas (? file)) data) (add data1 a)))
; ;
; ; (defmethod add ((data1 data) (a cons)) (add data1 (coerce a 'vector)))
; ;
; ; (defmethod add ((data1 data) (v vector))
; ;   (with-slots (cols row) data1
; ;     (if  cols
; ;       (push (dolist (col cols v) (add col (elt v (? col at)))) rows)
; ;       (setf cols (make-cols v)))))
; ;
