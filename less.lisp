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
  (if fs `(o (slot-value ,struct ',f) ,@fs) `(slot-value ,struct ',f)))  

(defmacro inca (x lst &optional (init 0))
  `(incf (cdr (or (assoc ,x ,lst :test #'equal) 
            (car (setf ,lst (cons (cons ,x ,init) ,lst)))))))

;--- col
(defun make-col (&key (at 0) (txt " "))
  (if (upper-case-p (elt txt 0)) 
    (make-num :at at :txt txt) 
    (make-sym :at at :txt txt)))

(defstruct sym  (at 0) (txt " ") (n 0)  has mode (most 0))
(defstruct (num (:constructor %make-num)) 
   (lo 1e30) (hi -1e30) (mu 0) (at 0) (txt " ") (n 0) (m2 0) (heaven 1))

(defun make-num (&key (at 0) (txt " "))
  (%make-num :at at :txt txt  :heaven (if (eq #\- (last-char txt)) 0 1)))

(defmethod adds (col1 lst)
  (dolist (x lst col1) (add col1 x)))

(defmethod add ((self num) x)
  (with-slots (lo hi n mu m2) self
    (unless (eq #\? x)
      (incf n)
      (let ((d (- x mu)))
        (incf mu (/ d n))
        (incf m2 (* d (-  x mu)))
        (setf lo (min x lo)
              hi (max x hi))))))

(defmethod add ((sym1 sym) x)
  (with-slots (n has most mode) sym1
    (unless (eq x '?)
      (incf n)
      (let ((new (inca x has)))
         (if (> new most)
          (setf mode x 
                most new))))))

(defmethod div ((num1 num)) (sqrt (/ (num-m2 num1) (- (num-n num1) 1))))
(defmethod div ((sym1 sym))
  (with-slots (has n) sym1
    (* -1 (loop :for (_ . v) :in has :sum  (* (/ v n) (log (/ v n) 2))))))

(defmethod mid ((num1 num)) (num-mu num1))
(defmethod mid ((sym1 sym)) (sym-mode sym1))
;--- data -------------------------------------------------------
(defstruct (data (:constructor %make-data)) rows cols)
(defstruct (cols (:constructor %make-cols)) x y all names)

(defun make-data (str &aux (data1 (%make-data)))
  (if (stringp str)
    (with-csv str (lambda (row) (add data1 row)))
    (dolist (row str) (add data1 row)))
  data1)

(defmethod add ((data1 data) row)
  (with-slots (rows cols) data1
    (if cols 
      (push (add cols row) rows) 
      (setf cols (make-cols row)))))
            
(defun make-cols (names)   
  (let* (x y (n -1)
         (all (loop :for s :in names :collect (make-col :at (incf n) :txt s))))
    (dolist (col1 all (%make-cols :names names :all all :x x :y y))
      (when (not (eq #\X (last-char (o col1 txt))))
        (if (member (last-char (o col1 txt)) '(#\+ #\-))
          (push col1 y)
          (push col1 x))))))

(defmethod add ((cols1 cols) row)
  (with-slots (x y) cols1
    (dolist (tmp (list x y) row)
      (dolist (col tmp) 
        (add col (elt row (o col at)))))))

(defmethod stats ((data1 data) &key (rows (data-rows data1)) 
                                    (what #'mid) (digits 2) (cols 'y))
  (list (cons "N" (length rows))
        (loop :for col :in (slot-value (o data1 cols) cols)
              :collect (cons (o col txt) (rnd2 (funcall what col) digits)))))

;--- lib --------------------------------------------------------
;--- system specific stuff 
(defun args    ()  #+clisp ext:*args*   #+sbcl sb-ext:*posix-argv*)
(defun goodbye (x) #+clisp (ext:exit x) #+sbcl (sb-ext:exit :code x))

;---- settings  
(defun help-txt (lst)
  (with-output-to-string (s)
    (format s "~a~%~{~a~%~}" (car lst) (mapcar #'%help-txt (cdr lst)))))

(defun %help-txt (lst)
  (destructuring-bind (flag key help value) lst
    (format nil "    ~4a ~3a ~22a = ~a" flag 
      (typecase value (integer "I") (number "F") (string "S")(t "")) help value)))

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
(defun keysort (lst fun order)
  (mapcar #'cdr
    (sort 
      (mapcar (lambda (x) (cons (funcall fun x) x)) lst) 
      order :key #'car)))

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

(defmethod rnd2 (x  &optional (digits 2)) x)
(defmethod rnd2 ((num number) &optional (digits 2))
  (let* ((div (expt 10 digits))
         (tmp (/ (round (* num div)) div)))
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

(defun main (&optional fun)
  (labels ((use (x) (member (? eg) `("all" ,(subseq (down-name x) 3)) :test #'string=)))
    (let ((help (help-txt +about+)))
      (setf *options* 
        (loop :for (k key help v) :in (cdr +about+) :collect (list k flag v)))
      (if fun  
        (setf *options* (funcall fun *options*)))
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

(defun eg-keysort ()
  (let ((lst '(7 6 5 4 3 2 1)))
   (print (let ((n 0)) 
      (keysort lst #'(lambda (x) (incf n) (* -1 x)) #'<)
      n))
    (print lst)))

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
  (let ((sym1 (adds (make-sym) '(a a a a b b c))))
    (and (eql 'a (mid sym1)) (< 1.378 (div sym1) 1.388))))

(defun eg-num()
  (let ((num1 (adds (make-num) (loop :repeat 1000 :collect (normal 10 2)))))
    (and (< 9.9 (mid num1) 10.1) (< 1.9 (div num1) 2.1))))

(defun eg-cols ()
  (dolist (col (o (make-cols '("Name" "Age" "married" "Weight-")) all))
    (print col)))
    
(defun eg-data()
  (print (first (o (make-data (? file)) cols y))))

(defun eg-data()
  (print (stats (make-data (? file)))))

; ---------------------------------------------------------------
(main #'cli)
