;;;; fast optimizer
; vim : set ts=2 sw=2 sts=2

(defstruct about
  (what      "core.lisp")
  (why       "find independent values that select for best dependent values")
  (when      2024)
  (who       "Tim Menzies")
  (copyright "BSD-2")
  (egs       '("EG"))) ; a function EG-X enables a CLI option -X (with 1 optional argument)

(defstruct stats
  (bootstraps 512 )
  (cohen      0.35))

(defstruct config
  (seed  1234567891)
  (stats (make-stats))
  (about (make-about)))

(defvar *config* (make-config))

;;; macros
(set-macro-character #\$  #'(lambda (s _)
                              "turn `$x` into `(slot-value i 'x)"
                              `(slot-value i ',(read s t nil t))))

(defmacro ? (&rest fs) `(o *config* ,@fs))

(defmacro o (struct f &rest fs)
  "access nested slots; e.g. `(o *settings* stats cohen)`"
  (if fs `(o (slot-value ,struct ',f) ,@fs) `(slot-value ,struct ',f)))

(defmacro aif (test yes &optional no)
  "anaphoic if; results of `test` available to sub-form as the variable `it`"
  `(let ((it ,test)) (if it ,yes ,no)))

(defmacro inc (lst x &optional (init 0))
  "simple symbol counter memory; not recommended for more than 50 unique symbols)"
  `(incf (cdr (or (assoc ,x ,lst :test #'equal) 
              (car (setf ,lst (cons (cons ,x ,init) ,lst)))))))

;;; misc utils
(defun last-char (s) (char s (1- (length s))))

(defun args() #+clisp ext:*args*  #+sbcl sb-ext:*posix-argv*)

(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  (let ((it (let ((*read-eval* nil))
              (read-from-string s1 ""))))
    (cond ((numberp it)     it)
          ((eq it t)        t)
          ((eq it nil)      nil)
          ((string= it "?") '?)
          (t                s1))))

(defun split (s &optional (here 0))
  (let ((there (position #\, s :start here)))
    (cons (thing (subseq s here there))
          (if there (split s (1+ there))))))

(defun with-csv (&optional file (fun #'print) (filter #'split))
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))

(defun string-prefix-p (pre str &aux (n (length pre)))
  (and (<= n (length str)) (string= pre (subseq str 0 n))))

;;; structs
(defstruct data rows cols)

(defstruct (cols (:constructor %make-cols)) all x y names)

(defstruct col (pos 0) (txt " ") (n 0))

(defstruct (sym (:include col)) has mode (most 0))

(defstruct (num (:include col) (:constructor %make-num))
  (lo 1e30) (hi -1e30) (mu 0) (m2 0) (goal 1))

;;; nums and syms
(defun make-num (&key (txt " ") (pos 0))
  (%make-num :pos pos :txt txt :goal (if (eq #\- (last-char txt)) 0 1)))

(defmethod add ((i col) x)
  (unless (eq #\? x) (incf $n) (add1 i x))
  x)

(defmethod add1 ((i num) x)
  (let ((d (- x $mu)))
    (incf $mu  (/ d $n))
    (incf $m2  (* d (-  x $mu)))
    (setf $lo  (min x $lo)
          $hi  (max x $hi))))

(defmethod add1 ((i sym) x)
  (let ((new (inc $has x)))
    (if (> new $most)
      (setf $mode x 
            $most new))))

(defmethod mid ((i num)) $mu)
(defmethod mid ((i sym)) $mode)

(defmethod div ((i num)) (if (< $n 2) 0 (sqrt (/ $m2 (- $n 1)))))
(defmethod div ((i sym)) 
  (* -1 (loop :for (_ . v) :in $has :sum (* (/ v $n) (log (/ v $n) 2)))))

;;; data and cols
(defmethod clone ((i data) &optional inits)
  (from (make-data) (cons (o i cols names) inits)))
  
(defmethod from ((i data) (file string))
  (with-csv file (lambda (row) (add i row)))
  i)

(defmethod from ((i data) (rows cons))
  (dolist (row rows i) (add i row)))

(defmethod add ((i data) row)
  (if $cols 
      (push (add $cols row) $rows) 
      (setf $cols (make-cols row))))

(defun make-cols (names  &aux (i (%make-cols :names names)))   
  (dolist (name names i)
    (make-cols1 i name (last-char name)
                (if (upper-case-p (char name 0)) #'make-num #'make-sym))))
                          
(defmethod make-cols1 ((i cols) name z ako)
  (let ((col (funcall ako :txt name :pos (length $all))))
    (push col $all)
    (when (not (eq z #\X))
      (if (member z '(#\+ #\-))
          (push col $y)
          (push col $x)))))

(defmethod add ((i cols) row)
  (dolist (cs (list $x $y) row)
    (dolist (col cs)
      (add col (elt row $pos)))))

;;; start-up support
(defun make () #+sbcl (setf sb-ext:*muffled-warnings* 'style-warning) (load "core"))

(defmethod help ((i about))
  (format t "~a : ~a~%(c)~a ~a ~a~%~%OPTIONS:~%" $what $why $when $who $copyright))

(defmethod help ((i config))
  (help $about)
  (let ((tmp (loop for s being the symbols of *package* collect (list s (symbol-name s)))))
        (dolist (pre (o $about egs))
          (loop :for (sym name) :in (sort tmp #'string< :key #'first)
                :if  (and (fboundp sym) (string-prefix-p pre name))
                :do  (format t " ~(~7a~) ~a~%" (subseq name (length pre))
                                               (documentation sym 'function))))))

(defmethod main ((i config))  
  (loop :for (flag arg) :on (args) :by #'cdr :do
    (dolist (pre (o $about egs))
      (aif (fboundp (intern (format nil "~a~:@(~a~)" pre flag)))
           (funcall it (thing arg))))))

;;; start up
;; Every function eg-x enables a command line flag -x with one optional argument
(defun eg-h (_ &aux tmp)
  "show about help and the doco from the eg functions"
  (help *config*))

(defun eg-num(_)
  "test NUMs"
  (let ((num (make-num)))
    (assert (< 0.46 (div (dotimes (n 1000 num) (add num (sqrt n)))) 0.47))))

(defun eg-sym(_)
  "test SYMs"
  (let* ((sym (make-sym)))
    (dolist (char '("a" "a" "a" "a" "b" "b" "c")) (add sym char))
    (assert (< 1.37 (div sym) 1.38))))

(main *config*)
