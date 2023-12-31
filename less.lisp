; ## Config
; About

(defvar +about+ "
LESS: less is more
(c)2023 Tim Menzies <timm.ieee.org> BSD-2
  
USAGE:
    sbcl --script tiny.lisp [OPTIONS]
    clisp less.lisp [OPTIONS]")

; Options:

(defvar *options* '(
  (BOOTSTRAPS "-b"  "number of bootstraps"                  256)
  (BOOTCONF   "-B"  "bootstrap threshold"                   .05) 
  (COHEN      "-d"  "Cohen delta"                           .35)
  (EG         "-e"  "start up actions"                "nothing")
  (FILE       "-f"  "data file"            "../data/auto93.csv")
  (HELP       "-h"  "show help"                             nil)
  (P          "-p"  "distance coeffecient"                    2)
  (SEED       "-s"  "random seed"                         10013)))

; ## Macros 
; Option macros

(defmacro ? (key) `(fourth (assoc ',key *options*)))

; Print help                                        ;

(defun print-help ()
  (format t "~a~%~%OPTIONS:~%" +about+)
  (loop :for (_ flag help value) :in *options* :do
    (format t "    ~4a ~3a ~22a = ~a~%" flag 
      (typecase value (integer "I") (number "F") (string "S")(t ""))
      help value)))

; Nested slot accessors.

(defmacro o (struct f &rest fs)
  (if fs `(o (slot-value ,struct ',f) ,@fs) `(slot-value ,struct ',f)))  

; Simple frequency counter.

(defmacro inca (x lst &optional (init 0))
  `(incf (cdr (or (assoc ,x ,lst :test #'equal) 
              (car (setf ,lst (cons (cons ,x ,init) ,lst)))))))

; Anaphoric if                                        ;

(defmacro aif (test-form then-form &optional else-form) 
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))


(defmethod last-char ((s string)) (char s (1- (length s))))
(defmethod last-char ((s symbol)) (last-char (symbol-name s)))

; ## Columns

(defstruct sym  (at 0) (txt " ") (n 0)  has mode (most 0))
(defstruct (num (:constructor %make-num)) 
   (lo 1e30) (hi -1e30) (mu 0) (at 0) (txt " ") (n 0) (m2 0) (heaven 1))

(defun make-num (&key (at 0) (txt " "))
  (%make-num :at at :txt txt  :heaven (if (eq #\- (last-char txt)) 0 1)))

(defun make-col (&key (at 0) (txt " "))
  (if (upper-case-p (elt txt 0)) 
    (make-num :at at :txt txt) 
    (make-sym :at at :txt txt)))

(defmethod adds (col1 lst)
  (dolist (x lst col1) (add col1 x)))

(defmethod add ((num1 num) x)
  (with-slots (lo hi n mu m2) num1
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

(defmethod mid ((num1 num)) (num-mu num1))
(defmethod mid ((sym1 sym)) (sym-mode sym1))

(defmethod div ((num1 num)) (sqrt (/ (num-m2 num1) (- (num-n num1) 1))))
(defmethod div ((sym1 sym))
  (with-slots (has n) sym1
    (* -1 (loop :for (_ . v) :in has :sum  (* (/ v n) (log (/ v n) 2))))))

; ## Data 
(defstruct (data (:constructor %make-data)) rows cols)
(defstruct row cells)
(defstruct (cols (:constructor %make-cols)) x y all names)

(defun make-data (str &aux (data1 (%make-data)))
  (if (stringp str)
    (with-csv str (lambda (row)  (add data1 row)))
    (dolist (row str) (add data1 row)))
  data1)

(defmethod add ((data1 data) (cells cons))
  (add data1 (make-row :cells cells)))

(defmethod add ((data1 data) (row1 row))
  (with-slots (rows cols) data1
    (if cols 
      (push (add cols row1) rows) 
      (setf cols (make-cols (row-cells row1))))))
            
(defun make-cols (names)   
  (let (all x y (n -1))
    (dolist (col1 (loop :for s :in names :collect (make-col :at (incf n) :txt s)) 
                  (%make-cols :names names :all all :x x :y y))
      (push col1 all)
      (when (not (eq #\X (last-char (o col1 txt))))
        (if (member (last-char (o col1 txt)) '(#\+ #\-))
          (push col1 y)
          (push col1 x))))))

(defmethod add ((cols1 cols) (row1 row))
  (with-slots (x y) cols1
    (dolist (tmp (list x y) row1)
      (dolist (col tmp) 
        (add col (elt (row-cells row1) (o col at)))))))

(defmethod stats ((data1 data) &key (rows (data-rows data1)) 
                                    (what #'mid) (digits 3) (cols 'y))
  (cons (cons "N" (length rows))
        (loop :for col :in (slot-value (o data1 cols) cols)
              :collect (cons (o col txt) 
                             (rnd2 (funcall what col) digits)))))

;--- lib --------------------------------------------------------
;--- system specific stuff 
(defun args    ()  #+clisp ext:*args*   #+sbcl sb-ext:*posix-argv*)
(defun goodbye (x) #+clisp (ext:exit x) #+sbcl (sb-ext:exit :code x))

;---- settings 
;--- strings2 things   
(defun read-safely-from-string (s)
  (let ((*read-eval* nil)) (read-from-string s "")))

(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  (let ((it (read-safely-from-string s1)))
    (cond ((numberp it)     it)
          ((eq it t)        it)
          ((eq it nil)      nil)
          ((string= it "?") '?)
          (t                s1)))); else return nil

;--- update settings from the command line values
(defun cli (lst)
  (loop :for (key flag help b4) :in lst :collect
    (list key flag help (aif (member flag (args) :test #'string=)
                          (cond ((eq b4 t)   nil)
                                ((eq b4 nil) t)
                                (t (thing (second it))))
                          b4))))

;---- lists
(defun keysort (lst fun order)
  (mapcar #'cdr (sort (mapcar (lambda (x) (cons (funcall fun x) x)) lst) 
                      order :key #'car)))

;---- strings 
(defun down-name (x) (string-downcase (symbol-name x)))

(defun split (s &optional (here 0))
  (let ((there (position #\, s :start here)))
     (cons (thing (subseq s here there))
       (if there (split s (1+ there))))))

(defun with-csv (file &optional (fun #'print) (filter #'split))
  (with-open-file (s (or file  *standard-input*))
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))

;--- maths
(defun normal (&optional (mu 0) (sd 1)) 
  (+ mu (* sd (sqrt (* -2 (log (rand)))) (cos (* 2 pi (rand))))))

(defmethod rnd2 (x &optional (digits 2))
  (if (numberp x)
      (let* ((div (expt 10 digits))
             (tmp (/ (round (* x div)) div)))
        (if (zerop digits) (floor tmp) (float tmp)))
      x))

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

(defun few (seq &optional (n 1))
  (subseq (shuffle seq) 0 n))   

;---- examples
(defun egs()
  (labels ((eg (s) (equalp "eg-" (subseq s 0 (min 3 (length s))))))
    (loop :for x :being :the symbols :in *package* :if (eg (down-name x)) :collect x)))

(defun run (sym &aux (b4 (copy-tree *options*)))
  (setf *seed* (? seed))
  (let ((passed (funcall sym)))
    (setf *options* (copy-tree b4))
    (unless passed (format t "❌ FAIL : ~(~a~)~%" sym))
    passed))

(defun run-p(x)
  (member (? eg) `("all" ,(subseq (down-name x) 3)) :test #'string=))

(defun main (&optional update)
  (if update  (setf *options* (cli *options*)))
  (if (? help)
    (print-help)
    (goodbye (1- (loop :for eg :in (egs) :if (run-p eg) :count (not (run eg)))))))

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
  (dolist (col (o (make-cols '("Name" "Age" "married" "Weight-")) all) t)
   (print col)))

(defun eg-data(    &aux (n 0))
  (print (stats (make-data (? file)))))

; -------------------------------------------------------------
(main t)
