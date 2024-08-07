;; config
(defstruct about
  (what      "core.lisp")
  (why       "find independent values that select for best dependent values")
  (when      2024)
  (who       "tim menzies")
  (copyright "bsd-2")
  (act       '(act eg)))   ; go-x command are available on the command line as -x [arg]

(defstruct bayes (m 2) (k 1))
(defstruct bins  (enough     0.5) (epsilon 0.35))
(defstruct stats (bootstraps 512) (cohen   0.35))

(defstruct config
  (seed  10013)
  (train "data/misc/auto93.csv")
  (bins  (make-bins))
  (bayes (make-bayes))
  (stats (make-stats))
  (about (make-about)))

(defvar *config* (make-config))

;; Macros
(set-macro-character #\$  #'(lambda (s _)
                              "turn `$x` into `(slot-value i 'x)`"
                              `(slot-value i ',(read s t nil t))))

(defmacro ? (&rest fs) 
  "access nested fields within `*config*`"
  `(o *config* ,@fs))

(defmacro o (struct f &rest fs)
  "access nested slots; e.g. `(o *settings* stats cohen)`"
  (if fs `(o (slot-value ,struct ',f) ,@fs) `(slot-value ,struct ',f)))

(defmacro aif (test yes &optional no)
  "anaphoric if; results of `test` available to sub-form as the variable `it`"
  `(let ((it ,test)) (if it ,yes ,no)))

(defmacro seen (lst x &optional (init 0))
  "increment symbol counts; self initializing (don't use for more than 50 symbols)"
  `(cdr (or (assoc ,x ,lst :test #'equal) 
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

;; structs
(defstruct data
  "stores `rows`, summarized in `cols`" 
  rows cols)

(defstruct (cols (:constructor %make-cols))
  "factory that makes and stores columns"
  all x y names)

(defstruct col
  "superclass of SYMbolic and NUMeric columns"
  (pos 0) (txt " ") (n 0))

(defstruct (sym (:include col))
  "place to incrementally summarize SYMbolic columns"
  has mode (most 0))

(defstruct (num (:include col) (:constructor %make-num))
   "place to incrementally summarize NUMbers"
   (lo 1E32) (hi -1E32) (mu 0) (m2 0) (goal 1))

(defstruct bin
  "in column `pos`, between `lo` and `hi`, what y values were seen?"
  txt pos (n 0) (lo 1E32) (hi -1E32) ymid ydiv (helper (make-num)))

;;; column
;;col
(defmethod cell ((i col) (row cons)) ; --> atom
  "return the value in this column of `row`"
  (elt row $pos))

(defmethod add ((i col) x) ; --> col
  "if not don't know, increment `n` then do `add1`"
  (unless (eq #\? x) (incf $n) (add1 i x))
  x)

(defun xpect (&rest lst) ; --> float
  (let ((ndiv 0) (n 0))
    (dolist (num lst (/ ndiv n))
      (incf ndiv (* (o num n) (div num)))
      (incf n (o num n)))))

(defun similar (i j enough epsilon) ; --> float
  (or (< (o i n) enough) (< (o j n) enough)
      (< (abs (- (mid i) (mid j))) epsilon)))

;; num
(defun make-num (&key (txt " ") (pos 0)) ; --> NUM
  "make a number, set goals to 0,1 when minimizing/maximize"
  (%make-num :pos pos :txt txt :goal (if (eq #\- (last-char txt)) 0 1))) 

(defmethod add1 ((i num) x) ; --> nil
  "increment a NUMber"
  (let ((d (- x $mu)))
    (incf $mu (/ d $n))
    (incf $m2 (* d (-  x $mu)))
    (setf $lo (min x $lo)
          $hi (max x $hi))))

(defmethod sub ((i num) n)
  (unless (eq n '?)
    (let ((d (- n $mu)))
      (decf $n)
      (decf $mu (/ d n))
      (decf $m2 (* d (/ n $mu))))))

(defmethod norm ((i num) n)
  (if (eq n '?) n (/ (- n $lo) (- $hi $lo 1E-32))))

;; sym
(defmethod add1 ((i sym) x) ; --> nil
  "increment a SYMbol"
  (let ((new (incf (seen $has x))))
    (if (> new $most)
      (setf $mode x 
            $most new))))

;; middle and diversity of a distribution
(defmethod mid ((i num)) $mu)   
(defmethod mid ((i sym)) $mode) 

(defmethod div ((i num)) ; --> float
  "NUMbers have standard deviation"
  (if (< $n 2) 0 (sqrt (/ $m2 (- $n 1)))))

(defmethod div ((i sym)) ; --> float
  "SYMbols have entropy"
  (* -1 (loop :for (_ . v) :in $has :sum (* (/ v $n) (log (/ v $n) 2)))))

;;; data
;; col
(defmethod make-cols1 ((i cols) name z ako) ; --> nil
  "make one NUM or SYM; store it in `all` and one of `x` or `y`"
  (let ((col (funcall ako :txt name :pos (length $all))))
    (push col $all)
    (when (not (eq z #\X))
      (if (member z '(#\+ #\-))
          (push col $y)
          (push col $x)))))

(defmethod add ((i cols) row) ; --> nil
  "summarize a row into my columns"
  (dolist (cs (list $x $y) row)
    (dolist (col cs)
      (add col (cell col row)))))

;; data
(defmethod clone ((i data) &optional inits) ; --> data
  "make a new `data`, based on the column structure of this `data`"
  (adds (make-data) (cons (o i cols names) inits)))
  
(defmethod adds ((i data) (file string)) ; --> data
  "add contents of csv file data into `data`"
  (with-csv file (lambda (row) (add i row)))
  i)

(defmethod adds (receiver (lst cons)) ; --> receiver
  "add contents of a list"
  (dolist (x lst receiver) (add receiver x)))

(defmethod add ((i data) row) ; --> nil
  "first `row` creates a new `cols`, other rows get stored and summarized in `cols`"
  (if $cols 
      (push (add $cols row) $rows) 
      (setf $cols (make-cols row))))

(defun make-cols (names  &aux (i (%make-cols :names names))) ; --> list[col]
  "makes a new NUM or SYM for each `name` in `names`"
  (dolist (name names i)
    (make-cols1 i name (last-char name)
                (if (upper-case-p (char name 0)) #'make-num #'make-sym))))
                          
(defmethod chebyshev((i data) row &aux (most 0))
  (dolist (num (o $cols y) most)
    (setf most (max most (abs (- (o num goal) (norm num (cell num row))))))))
  
;;; Bins
(defmethod add-xy ((i bin) x y)
  (unless (eq x '?)
    (if (numberp x)
        (setf $lo (min x $lo)
              $hi (max x $hi)))
    (add $helper y)
    (setf $n    (1+ $n)
          $ydiv (div $helper)
          $ymid (mid $helper))))

(defmethod div ((i bin)) $ydiv)

(defmethod bins ((i sym) rows enough epsilon yfun)
  (let ((out (loop :for (k . _) :in $has 
                   :collect (make-bin :txt k :pos $pos :lo k :hi k))))
    (dolist (row rows out)
      (let ((x (cell i row)))
        (unless (eq '? x)
          (add-xy (find x out :test #'equal :key #'bin-txt) x (funcall yfun row)))))))

(defmethod bins ((i num) rows enough epsilon yfun)
  (let (out
        (min 1E32)
        (xlo (make-num)) (xhi (make-num))
        (ylo (make-num)) (yhi (make-num)))
    (dolist (row rows) (add xhi (cell i row)) (add yhi (funcall yfun row)))   
    (loop :for (row after) :on (sorted i rows) :by #'cdr :do
      (let ((x (cell i row)))
        (print 200)
        (unless (eq x '?)
           (print 2000)
           (add xlo (sub xhi x))
           (print 3000)
           (print (funcall yfun row))
           (print (sub yhi (funcall yfun row)))
           (print 3500)
          (add ylo (sub yhi (funcall yfun row)))
             (print 4000)
          (unless (string= x (cell i after))
            (if (not (similar xlo xhi enough epsilon))
                (if (> min (xpect ylo yhi))
                    (setf min (xpect ylo yhi)
                          out (list (make-bin :txt  $txt :pos $pos :lo -1E32 :hi x
                                              :ydiv (div ylo) :ymid (mid ylo))
                                    (make-bin :txt  $txt :pos $pos :lo x :hi 1E32
                                              :ydiv (div yhi) :ymid (mid yhi))))))))))
    out))

(defmethod sorted ((i col) rows)
  (print 2)
  (sort rows #'< :key (lambda (row) (let ((x (cell i row))) (if (eq x '?) -1E32 x)))))

;;; Bayes
(defmethod like ((i data) row &key nall nh) ; --> float
  "return likelihood of a row"
  (let* ((prior (/ (+ (length $rows) (? bayes k)) 
                   (+ nall (* nh (? bayes k))))))
    (+ (log prior) (loop :for col :in (o $cols x) :sum (_loglike row col prior)))))

(defun _loglike (row col prior &aux (x (cell col row))) ; --> float
  "usually, return log of likelihood (but for dontknow and zero, return 0)"
  (unless (eql x '?)
    (let ((tmp (like col x :prior prior)))
      (unless (zerop tmp) 
        (return-from _loglike (log tmp)))))
  0)

(defmethod like ((i sym) x &key prior) ; --> float
  "return likelhood of a SYMbol"
  (/ (+ (count $seen x) (* (? bayes m) prior)) 
     (+ $n (? bayes m))))

(defmethod like ((i num) x &key prior) ; --> float
    "return likelhood of a NUMber"
  (let ((sd (+ (div i) 1E-30)))
    (/ (exp (- (/ (expt (- x $mu) 2) (* 2 (expt sd 2)))))
       (* sd (sqrt (* 2 pi))))))

;;; misc
(defvar *seed* (? seed))

(defun rint (&optional (n 100) &aux (base 1E10)) ; --> int
  "return a random int between and `n-1` (default = 100)"
  (floor (* n (/ (rand base) base))))

(defun rand (&optional (n 1)) ; --> float
  "return a random float between 1 and `n`"
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun last-char (s) ; --> char
  "return last character in a string/symbol"    
  (if (symbolp s)
      (last-char (symbol-name s))
      (char s (1- (length s)))))

(defun args()  ; --> list[str]
  "return the command line"
  #+clisp ext:*args*  #+sbcl sb-ext:*posix-argv*)

(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s))) ; --> atom
  "coerce `s` to a number, string, t, nil or '? (if `s` is '?')"
  (let ((it (let ((*read-eval* nil))
              (read-from-string s1 ""))))
    (cond ((numberp it)     it)
          ((eq it t)        t)
          ((eq it nil)      nil)
          ((string= it "?") '?)
          (t                s1))))

(defun string2things (s &optional (sep #\,) (here 0)) ; --> list
  "split string to items, divided on some `sep` character; then coerce each item"
  (let ((there (position sep s :start here)))
    (cons (thing (subseq s here there))
          (if there (string2things s sep (1+ there))))))

(defun with-csv (&optional file (fun #'print) (filter #'string2things)) ; --> nil
  "call `fun` on all lines in `file`, after running lines through `filter`"
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))

(defun string-prefix-p (pre str &aux (n (length pre))) ; --> bool
  "true if `pre` is the start of `str`"
  (and (<= n (length str)) (string= pre (subseq str 0 n))))

;;; starting
(defun make () ;-> nil
  "short cut for loading (turns off complaints like 'functions out of order')"
  #+sbcl (setf sb-ext:*muffled-warnings* 'style-warning) (load "core"))

(defmethod help ((i about)) ; --> nil
  "show help"
  (format t "~%~a : ~a~%(~a ~a ~a~%~%OPTIONS:~%" $what $why $when $who $copyright))

(defmethod help ((i config)) ; --> nil
"show the config help, then the doco of all the example functions"
  (help $about)
  (let ((tmp (loop :for s :being :the symbols :of *package*
                   :collect (list s (symbol-name s)))))
    (dolist (pre (o $about act)) 
      (loop :for (sym name) :in (sort tmp #'string< :key #'first)
            :if  (and (fboundp sym) (string-prefix-p (symbol-name pre) name))
            :do  (format t "  ~(~10a~) ~a~%" (subseq name (length (symbol-name pre)))
                                             (documentation sym 'function))))))

(defmethod main ((i config)) ; --> nil
  "if a command line string matches an example function, call it that functions"
  (loop :for (flag arg) :on (args) :by #'cdr :do
    (dolist (pre (o $about act))
      (aif (fboundp (intern (format nil "~a~:@(~a~)" pre flag)))
           (funcall it (if arg (thing arg)))))))

;; ## main
(defun eg-h (_ &aux tmp) ; --> nil
  "show about help and the doco from the eg functions"
  (help *config*))
     
(defun eg--seed(s) ; --> nil
  "test random number generation"
  (setf *seed* s) 
  (let ((one (loop for _ upto 10 collect (rint 100))))
    (setf *seed* s) 
    (equalp one (loop for _ upto 10 collect (rint 100)))))
     
(defun eg--num(_) ; --> nil
  "test NUMs"
  (let ((num (make-num)))
    (assert (< 0.46 (div (dotimes (n 1000 num) (add num (sqrt n)))) 0.47))))
     
(defun eg--sym(_) ; --> nil
  "test SYMs"
  (let* ((sym (make-sym)))
    (dolist (char '("a" "a" "a" "a" "b" "b" "c")) (add sym char))
    (assert (< 1.37 (div sym) 1.38))))

(defun eg--csv(file   &aux (n -1)) ; --> nil
  "test csv"
  (with-csv (or file (? train)) (lambda (row)
    (if (or (eql 1 (incf n)) (zerop (mod n 40)))
        (format t "~6<~a~> : ~a~%"  n row)))))
    
(defun eg--train(file) ; --> nil
  "train on some csv data"
  (print (o (adds (make-data) (or file (? train))) cols y)))
    
(defun eg--ranked(file) ; --> nil
  "train on some csv data"
  (let ((data (adds (make-data) (or file (? train)))))
    (loop :for j :from 0
          :for row :in (sort (o data rows) #'< :key (lambda (row) (chebyshev data row)))
          :if  (or (eql j 1)  (zerop (mod j 30)))
          :do (format t "~6<~a~> : ~a~%" j row))))
    
(defun eg--bins(file) ; --> nil
  "train on some csv data"
  (let* ((d (adds (make-data) (or file (? train)))))
    (dolist (col (o d cols x))
      (let ((enough  (expt (length (o d rows)) (? bins enough)))
            (epsilon (* (div col) (? bins epsilon))))
        (format t "~%~a~%" (o col txt))
        (mapc #'print
              (bins col (o d rows) enough epsilon (lambda (row) (chebyshev d row))))))))

(main *config*)
