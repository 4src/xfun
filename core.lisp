(defstruct about
  (what      "core.lisp")
  (why       "find independent values that select for best dependent values")
  (when      2024)
  (who       "Tim Menzies")
  (copyright "BSD-2")
  (egs       '("EG"))) ; list example function prefixes

(defstruct stats
  (bootstraps 512 )
  (cohen      0.35))

(defstruct config
  (seed  1234567891)
  (train "data/auto93.csv")
  (stats (make-stats))
  (about (make-about)))

(defvar *config* (make-config))

;;; macros
(set-macro-character #\$  #'(lambda (s _)
                              "turn `$x` into `(slot-value i 'x)"
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

(defmacro inc (lst x &optional (init 0))
  "increment symbol counts; self initializing (don't use for more than 50 symbols)"
  `(incf (cdr (or (assoc ,x ,lst :test #'equal) 
              (car (setf ,lst (cons (cons ,x ,init) ,lst)))))))

;;; misc utils
(defun last-char (s) 
  "return last character in a string"
  (char s (1- (length s))))

(defun args() 
  "return the command line"
  #+clisp ext:*args*  #+sbcl sb-ext:*posix-argv*)

(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  "coerce `s` to a number, string, t, nil or '? (if `s` is '?')"
  (let ((it (let ((*read-eval* nil))
              (read-from-string s1 ""))))
    (cond ((numberp it)     it)
          ((eq it t)        t)
          ((eq it nil)      nil)
          ((string= it "?") '?)
          (t                s1))))

(defun split (s &optional (here 0))
  "split a string on commas"
  (let ((there (position #\, s :start here)))
    (cons (thing (subseq s here there))
          (if there (split s (1+ there))))))

(defun with-csv (&optional file (fun #'print) (filter #'split))
  "call `fun` on all lines in `file`, after running lines through `filter`"
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))

(defun string-prefix-p (pre str &aux (n (length pre)))
  "true if `pre` is the start of `str`"
  (and (<= n (length str)) (string= pre (subseq str 0 n))))

;;; nums and syms
(defstruct col
  "superclass of SYM and NUM"
  (pos 0) (txt " ") (n 0))

(defstruct (sym (:include col))
  "place to incrementally summarize SYMbols"
  has mode (most 0))

(defstruct (num (:include col) (:constructor %make-num))
   "place to incrementally summarize NUMbers"
  (lo 1e30) (hi -1e30) (mu 0) (m2 0) (goal 1))

(defun make-num (&key (txt " ") (pos 0))
  "make a number, set goals to 0,1 when minimizing/maximize"
  (%make-num :pos pos :txt txt :goal (if (eq #\- (last-char txt)) 0 1)))

(defmethod add ((i col) x)
  "if not don't know, increment `n` then call `add`"
  (unless (eq #\? x) (incf $n) (add1 i x))
  x)

(defmethod add1 ((i num) x)
  "increment a NUMber"
  (let ((d (- x $mu)))
    (incf $mu  (/ d $n))
    (incf $m2  (* d (-  x $mu)))
    (setf $lo  (min x $lo)
          $hi  (max x $hi))))

(defmethod add1 ((i sym) x)
  "increment a SYMbol"
  (let ((new (inc $has x)))
    (if (> new $most)
      (setf $mode x 
            $most new))))

(defmethod mid ((i num)) "mean" $mu)   
(defmethod mid ((i sym)) "mode" $mode) 

(defmethod div ((i num))
  "numbers have standard deviation"
  (if (< $n 2) 0 (sqrt (/ $m2 (- $n 1)))))

(defmethod div ((i sym))
  "symbols have entropy"
  (* -1 (loop :for (_ . v) :in $has :sum (* (/ v $n) (log (/ v $n) 2)))))

;;; data and cols
(defstruct data
  "stores `rows`, summarized in `cols`"
  rows cols)

(defstruct (cols (:constructor %make-cols))
  "factory that makes and stores columns"
  all x y names)

(defmethod clone ((i data) &optional inits)
  "make a new `data`, based on the column structure of this `data`"
  (from (make-data) (cons (o i cols names) inits)))
  
(defmethod from ((i data) (file string))
  "load csv file data into `data`"
  (with-csv file (lambda (row) (add i row)))
  i)

(defmethod from ((i data) (rows cons))
  "load rows from a list into `data`"
  (dolist (row rows i) (add i row)))

(defmethod add ((i data) row)
  "first `row` creates a new `cols`, other rows get stored and summarized in `cols`"
  (if $cols 
      (push (add $cols row) $rows) 
      (setf $cols (make-cols row))))

(defun make-cols (names  &aux (i (%make-cols :names names)))
  "makes a new NUM or SYM for each `name` in `names`"
  (dolist (name names i)
    (make-cols1 i name (last-char name)
                (if (upper-case-p (char name 0)) #'make-num #'make-sym))))
                          
(defmethod make-cols1 ((i cols) name z ako)
  "make one NUM or SYM; store it in `all` and one of `x` or `y`"
  (let ((col (funcall ako :txt name :pos (length $all))))
    (push col $all)
    (when (not (eq z #\X))
      (if (member z '(#\+ #\-))
          (push col $y)
          (push col $x)))))

(defmethod add ((i cols) row)
  "summarize a row into my columns"
  (dolist (cs (list $x $y) row)
    (dolist (col cs)
      (add col (elt row (o col pos))))))

;;; start-up support
(defun make ()
  "short cut for loading (does not complain about functions out of order)"
  #+sbcl (setf sb-ext:*muffled-warnings* 'style-warning) (load "core"))

(defmethod help ((i about))
  "show help"
  (format t "~a : ~a~%(c)~a ~a ~a~%~%OPTIONS:~%" $what $why $when $who $copyright))

(defmethod help ((i config))
  "show the config help, then the doco of all the example functions"
  (help $about)
  (let ((tmp (loop for s being the symbols of *package* collect (list s (symbol-name s)))))
        (dolist (pre (o $about egs))
          (loop :for (sym name) :in (sort tmp #'string< :key #'first)
                :if  (and (fboundp sym) (string-prefix-p pre name))
                :do  (format t " ~(~7a~) ~a~%" (subseq name (length pre))
                                               (documentation sym 'function))))))

(defmethod main ((i config))
  "if a command line string matches an example function, call it that functions"
  (loop :for (flag arg) :on (args) :by #'cdr :do
    (dolist (pre (o $about egs))
      (aif (fboundp (intern (format nil "~a~:@(~a~)" pre flag)))
           (funcall it (if arg (thing arg)))))))

;;; start up
;; Every function eg-x enables a command line flag -x with one optional argument
(defun eg-h (_ &aux tmp)
  "show about help and the doco from the eg functions"
  (help *config*))

(defun eg--num(_)
  "test NUMs"
  (let ((num (make-num)))
    (assert (< 0.46 (div (dotimes (n 1000 num) (add num (sqrt n)))) 0.47))))

(defun eg--sym(_)
  "test SYMs"
  (let* ((sym (make-sym)))
    (dolist (char '("a" "a" "a" "a" "b" "b" "c")) (add sym char))
    (assert (< 1.37 (div sym) 1.38))))

(defun eg--csv(file)
  (with-csv (or file (? train)) #'identity))

(defun eg--train(file)
  (print (o (from (make-data) (or file (? train))) cols y)))

(main *config*)
