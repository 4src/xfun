(defvar +about+
  '("
LESS: less is more
(c)2023 Tim Menzies <timm.ieee.org> BSD-2

USAGE:
    sbcl --script tiny.lisp [OPTIONS]
    clisp less.lisp [OPTIONS]
     
OPTIONS:"
  ("-b"  BOOTSTRAPS  "number of bootstraps"           256)
  ("-B"  BOOTCONF    "bootstrap threshold"            .05) 
  ("-d"  COHEN       "Cohen delta"                    .35)
  ("-e"  EG          "start up actions"         "nothing")
  ("-f"  FILE        "data file"     "../data/auto93.csv")
  ("-h"  HELP        "show help"                      nil)
  ("-p"  P           "distance coeffecient"             2)
  ("-s"  SEED        "random seed"                  10013)))

;--- macros  (must go first) -------------------------------------
;--- optionMacros
(defvar *options* nil) ;  filled in later by `settings`
(defmacro ? (key) `(third (assoc ',key *options*)))

;--- generalMacros 
(defmacro o (struct f &rest fs)
  (if fs `(o (slot-value ,struct ',f) ,@fs) `(f-value ,struct ',f)))  

(defmacro inc (x lst &optional (init 0))
  `(cdr (or (assoc ,x ,lst :test #'equal) 
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

;--- col
(defstruct col
  (at 0) (txt " ") (n 0) has)

(defun col0 (at &optional (txt " "))
  (if (upper-case-p (elt txt 0)) (make-num at txt) (make-sym at txt)))

(defstruct (sym (:include col)) mode (most 0))
(defstruct (num (:include col) 
                (:constructor %make-num)) ok (heaven 1))

(defun make-num (&optional (at 0) (txt " "))
  (%make-num :at at :txt txt  :heaven (if (eq #\- (last-char txt)) 0 1)))

(defmethod add ((col1 col) (lst cons))
  (dolist (x lst col1) (add col1) x))

(defmethod add ((num1 num) x)
  (with-slots (n has ok) num1
    (unless (eql x '?)
      (incf n)
      (push x has)
      (setf ok nil))))

(defmethod add ((sym1 sym) x)
  (with-slots (n has most mode) sym1
    (unless (eql x '?)
      (incf n)
      (let ((new (inc x has)))
         (if (> new most)
          (setf mode x 
                most new))))))

(defmethod seen ((sym1 sym)) (sym-has sym1))
(defmethod seen ((num1 num))
  (with-slots (has ok) num1
    (unless ok (sort has #'<))
    (setf ok t)
    has))

(defmethod per ((num1 num) &optional (p .5))
  (let ((a (seen num1))) (elt a (floor (* p (length a))))))

(defmethod div ((num1 num)) (/ (- (per num1 .9) (per num1 .1)) 2.56))
(defmethod div ((sym1 sym))
  (with-slots (has n) sym1
    (* -1 (loop :for (_ . v) :in has :sum  (* (/ v n) (log (/ v n) 2))))))

(defmethod mid ((num1 num)) (per num1 .5))
(defmethod mid ((sym1 sym)) (sym-mode sym1))

(defstruct (cols (:constructor %make-cols)) x y all names)

(defun make-cols (lst)
  (let* (x y (n -1)
         (all (mapcar (lambda (s) (make-col :at (incf n) :name s)) lst)))
    (dolist (col1 all (%make-cols :names lst :all all :x x :y y))
      (when (not (eq #\X (last-char (o col1 txt))))
        (if (member (last-char (o col1 txt)) '(#\+ #\-))
          (push col1 y)
          (push col1 x))))))

;--- lib --------------------------------------------------------
;--- system specific stuff 
(defun args    ()  #+clisp ext:*args*   #+sbcl sb-ext:*posix-argv*)
(defun goodbye (x) #+clisp (ext:exit x) #+sbcl (sb-ext:exit :code x))

;---- settings  
(defun opt (lst)
  (destructuring-bind (flag key help value) lst
    (push (list key flag value) *options*)
    (format nil "    ~4a ~3a ~22a = ~a" flag 
      (typecase value (integer "I") (number "F") (string "S")(t "")) help value)))

(defun settings (lst)
  (with-output-to-string (s)
    (format s "~a~%~{~a~%~}" (car lst) (mapcar #'opt (cdr lst)))))

;--- strings2 things                 
(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  (let* ((*read-eval* nil)
         (x (read-from-string s1)))
    (cond ((numberp x) x)
          ((eq x t)    t)
          ((eq x nil)  nil)
          ((string= x "?") '?)
          (t           s1))))

(defun cli (lst &aux it)
  (let ((args #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*))
    (loop :for (key flag b4) :in lst :collect
          (list key flag (if (setf it (member flag args :test #'string=))
                           (cond ((eq b4 t) nil)
                                 ((eq b4 nil) t)
                                 (t (thing (second it))))
                           b4)))))

;---- lists

;---- strings 
(defun down-name (x) (string-downcase (symbol-name x)))

(defmethod last-char ((s string)) (char s (1- (length s))))
(defmethod last-char ((s symbol)) (last-char (symbol-name s)))

(defun split (s &optional (here 0))
  (let* ((there (position #\, s :start here)))
      (cons (thing (subseq s here there))
            (if there (split s (1+ there))))))

(defun with-csv (file &optional (fun #'print) (filter #'split))
  (with-open-file (s (or file  *standard-input*))
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))

;--- maths
(defun normal (&optional (mu 0) (sd 1)) 
  (+ mu (* sd (sqrt (* -2 (log (rand)))) (cos (* 2 pi (rand))))))

(defun rnd2 (number &optional (digits 2))
  (let* ((div (expt 10 digits))
         (tmp (/ (round (* number div)) div)))
    (if (zerop digits) (floor tmp) (float tmp))))

;--- randoms
(defvar *seed* 10013)
(defun rand (&optional (n 1))
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 1) &aux (base 10000000000.0))
  (floor (* n (/ (rand base) base))))

(defmethod sample ((a cons) &optional (n (length a))) 
  (sample (coerce a 'vector) n))

(defmethod sample ((a vector) &optional (n (length a)))
  (let ((len (length a)))
    (loop :repeat n :collect (elt a  (rint len)))))

(defmethod shuffle ((a cons)) (coerce (shuffle (coerce a 'vector)) 'cons))
(defmethod shuffle ((a vector)) 
  (loop :for i :from (length a) :downto 2 
        :do (rotatef (elt a (rint i)) (elt a (1- i))))
  a)

(defun few (seq n)
  (subseq (shuffle seq) 0 n))

;---- examples
(defun egs()
  (labels ((eg (s) (equalp "eg-" (subseq s 0 (min 3 (length s)))))) 
    (loop :for x :being :the symbols :in *package* :if (eg (down-name x)) :collect x)))

(defun main ()
  (labels ((use (x) (member (? eg) `("all" ,(subseq (down-name x) 3)) :test #'string=)))
    (let ((help (settings +about+)))
      (setf *options* (cli *options*))  
      (if (? help)
        (princ help)
        (goodbye (1- (loop :for eg :in (egs) :if (use eg) :count (eq nil (run eg)))))))))

(defun run (sym &aux (b4 (copy-tree *options*)))
  (setf *seed* (? seed))
  (let ((passed (funcall sym)))
    (setf *options* (copy-tree b4))
    (unless passed (format t "❌ FAIL : ~(~a~)~%" sym))
    passed))

; ---------------------------------------------------------------
(defun eg-fail() nil)
(defun eg-the() (print (cdr *options*)))

(defun eg-csv (&aux (n 0)) 
  (with-csv (? file) (lambda (a) (incf n (length a))))
  (= n 3192))

(defun eg-rand ()  
  (let (a b)
    (setf *seed* 1) (setf a (sort (loop :repeat 10 :collect (rint 100)) #'<))
    (setf *seed* 1) (setf b (sort (loop :repeat 10 :collect (rint 100)) #'<))
    (equal a b)))

(defun eg-sample ()
  (let ((a '(a b c d e f g)))
    (loop repeat 10 do (format t "~{~a~}~%" (sample a)))
    (loop repeat 10 do (format t "~{~a~}~%" (few a 3))))
  t)

(defun eg-sym () 
  (let ((sym1 (add (make-sym) '(a a a a b b c))))
    (and (eql 'a (mid sym1)) (< 1.378 (div sym1) 1.388))))

(defun eg-num()
  (let ((num1 (add (make-num) (loop :repeat 10000 :collect (normal 10 2)))))
    (and (< 9.95 (mid num1) 10.05) (< 1.95 (div num1) 2.05))))

; ---------------------------------------------------------------
(main)
 

;; ; ;
;; ; ; (defstruct (cols (:constructor %make-cols)) all x y klass names)
;; ; ;
;; ; ; (defmacro (data (:constructor %make-data)) rows cols)
;; ; ;
;; ; ; (defun make-cols (lst &aux (n -1) (cols0 (%make-cols :names lst)))
;; ; ;   (with-slots (all x y klass) cols0
;; ; ;     (setf all (mapcar (lambda (s) (make-col :at (incf n) :name s)) lst))
;; ; ;     (dolist (col all cols0)
;; ; ;       (when (not (eq #\X (last-char (o col name))))
;; ; ;         (if (member (last-char (o col name)) '(#\+ #\-))
;; ; ;           (push col (o cols1 y))
;; ; ;           (push col (o cols1 x)))))))
;; ; ;
;; ; ; (defun make-data (s &aux (data1 (%make-data :names s)))
;; ; ;   (with-slots (all x y klass) data1
;; ; ;     (setf all (mapcar 
;; ; ;  (dolist (a (datas (? file)) data) (add data1 a)))
;; ; ;
;; ; ; (defmethod add ((data1 data) (a cons)) (add data1 (coerce a 'vector)))
;; ; ;
;; ; ; (defmethod add ((data1 data) (v vector))
;; ; ;   (with-slots (cols row) data1
;; ; ;     (if  cols
;; ; ;       (push (dolist (col cols v) (add col (elt v (? col at)))) rows)
;; ; ;       (setf cols (make-cols v)))))
;; ; ;
