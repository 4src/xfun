(defvar +about-help+ "
LESS.lisp : less is more
(c)2023 Tim Menzies <timm.ieee.org> BSD-2

USAGE :
     sbcl --script tiny.lisp OPTIONS
     clisp less.lisp OPTIONS")

(defvar +about-options+
  '(("-b" BOOTSTRAPS "number of bootstraps"           256)
    ("-B" BOOTCONF   "bootstrap threshoa"             .05) 
    ("-d" COHEN      "Cohen delta"                    .35)
    ("-e" EG         "start up actions nothing" "nothing")
    ("-f" FILE       "data file"    "../data/auto93.lisp")
    ("-h" HELP       "show help"                      nil)
    ("-p" P          "distance coeffecient"             2)
    ("-s" SEED       "random seed"                   7001)))

(defconstant +help+    "") ; to be filled in later
(defvar      *options* nil) ; to be filled in later

;--- macros (must go first) --------------------------------------
(defmacro ? (key) `(third (assoc ',key *options*)))

(defmacro o (struct f &rest fs)
  (if fs `(o (slot-value ,struct ',f) ,@fs) `(f-value ,struct ',f)))  

(defmacro seen (x lst &optional (init 0))
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

;--- lib --------------------------------------------------------
;--- system specific stuff 
(defun args ()
  #+clisp ext:*args* 
  #+sbcl sb-ext:*posix-argv*)

(defun goodbye (&optional (x 0))
  #+clisp (ext:exit x)
  #+sbcl  (sb-ext:exit :code x))

;---- settings  
(defun settings (lst))
  (labels ((opt (lst1)
    (destructuring-bind (flag key help value) lst1
      (push (list key flag value) *opts*)
      (format nil "  ~4a ~3a ~30a = ~a" flag 
        (typecase value (integer "I") (number "F") (string "S")(t "")) help value))))
    (with-output-to-string (s)
      (format s "~a~%OPTIONS:~%~{~a~%~}" +about+ (mapcar #'opt lst)))

;--- strings2 things                 
(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  (let* ((*read-eval* nil)
         (x (read-from-string s1)))
    (cond ((numberp x) x)
          ((eq x t)    t)
          ((eq x nil)  nil)
          (t           s1))))

(defun cli (lst &aux it)
  (let ((args #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*))
    (loop :for (key flag b4) :in lst :collect
      (list key flag (if (setf it (member flag args :test #'string=))
                      (cond ((eq b4 t) nil)
                            ((eq b4 nil) t)
                            (t (thing (second it))))
                      b4)))))

;---- strings 
(defun down-name (x) (string-downcase (symbol-name x)))

(defmethod last-char ((s string)) (char s (1- (length s))))
(defmethod last-char ((s symbol)) (last-char (symbol-name s)))

(defun split (s &optional (here 0))
  (let* ((there (position #\, s :start here))
         (cons (thing (subseq s here there)
                      (if there (split s (1+ there))))))))

;---- start uo
(setf *opts* (cli *opts*))
(if (? help) (princ +help+))
 


;---- examples
(defun egs()
  (labels ((eg (x) (equalp "eg-" (subseq (down-name x) 0 (min 3 (length (str x))))))) 
    (loop :for x :being :the symbols :in *package* :if (eg x) :collect x)))

 (defun run1 (sym &aux (b4 (copy-tree *settings*)))
   (setf *seed* (?  seed))
  (prog1 (funcall sym) (setf *settings* (copy-tree b4))))

(defun run ()
   (labels ((use (x) (member (? eg) `("all" ,(subseq (down-name x) 3)) :test #'string=)))
     (setf  *settings* (cli *settings*))
     (goodbye (loop :for eg :in (egs) :if (use eg) :count (not (tiny-run eg))))))

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
