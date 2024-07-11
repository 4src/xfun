;;;; fast optimizer
; vim : set ts=2 sw=2 sts=2
;;; main options
(defstruct about
  (what      "core.lisp")
  (why       "find independent values that select for best dependent values")
  (when      2024)
  (who       "Tim Menzies")
  (copyright "BSD-2")
  (egs       '("EG")))

(defstruct stats (boostraps 1234567891) (cohen 0.35))
(defstruct options (stats (make-stats)) (about (make-about)))

(defvar our (make-options))

;;; macros
(set-macro-character #\$ #'(lambda (s _) `(slot-value self ',(read s t nil t))))

(defmacro o (struct f &rest fs)
  (if fs `(o (slot-value ,struct ',f) ,@fs) `(slot-value ,struct ',f)))

(defmacro aif (test yes &optional no) 
  `(let ((it ,test)) (if it ,yes ,no)))

(defmacro has (lst x &optional (init 0))
  `(cdr (or (assoc ,x ,lst :test #'equal) 
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

;;; structs
(defstruct data rows cols)

(defstruct (cols (:constructor %make-cols)) all x y names)

(defstruct sym (pos 0) (txt " ") (n 0) seen mode (most 0))

(defstruct (num (:constructor %make-num))
  (pos 0) (txt " ") (n 0) (lo 1e30) (hi -1e30) (mu 0) (m2 0) (goal 1))

;;; nums and syms
(defun make-col (&key pos txt)
  (funcall (if (upper-case-p (char txt 0)) #'make-num #'make-sym) :pos pos :txt txt))

(defun make-num (&key (txt " ") (pos 0))
  (%make-num :pos pos :txt txt :goal (if (eq #\- (last-char txt)) 0 1)))

(defmethod add ((self num) x)
  (unless (eq #\? x)
    (incf $n)
    (let ((d (- x $mu)))
      (incf $mu  (/ d $n))
      (incf $m2  (* d (-  x $mu)))
      (setf $lo  (min x $lo)
            $hi  (max x $hi))))
  x)

(defmethod add ((self sym) x)
  (unless (eq x '?)
    (incf $n)
    (let ((new (incf (has $seen x))))
      (if (> new $most)
          (setf $mode x 
                $most new))))
  x)

(defmethod mid ((self num)) $mu)
(defmethod mid ((self sym)) $mode)

(defmethod div ((self num)) (if (< $n 2) 0 (sqrt (/ $m2 (- $n 1)))))
(defmethod div ((self sym))
  (* -1 (loop :for (_ . v) :in $seen :sum (* (/ v $n) (log (/ v $n) 2)))))

;;; data and cols
(defmethod clone ((self data) &optional inits)
  (from (make-data) (cons (o self cols names) inits)))
  
(defmethod from ((self data) (file string))
  (with-csv file (lambda (row) (add self row)))
  self)

(defmethod from ((self data) (rows cons))
  (dolist (row rows self) (add self row)))

(defmethod add ((self data) row)
  (if $cols 
      (push (add $cols row) $rows) 
      (setf $cols (make-cols row))))

(defun make-cols (names  &aux (self (%make-cols :names names)))   
  (dolist (name names self)
    (make-cols1 self name (last-char name)
                (if (upper-case-p (char name 0)) #'make-num #'make-sym))))
                          
(defmethod make-cols1 ((self cols) name z ako)
  (let ((col (funcall ako :txt name :pos (length $all))))
    (push col $all)
    (when (not (eq #\X z))
      (if (member z '(#\+ #\-))
          (push col $y)
          (push col $x)))))

(defmethod add ((self cols) row)
  (dolist (cs (list $x $y) row)
    (dolist (col cs)
      (add col (elt row $pos)))))

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

;;; start-up support
(defun make () #+sbcl (setf sb-ext:*muffled-warnings* 'style-warning) (load "core"))

(defmethod help ((self about))
  (format t "~a : ~a~%(c)~a ~a ~a~%~%OPTIONS:~%" $what $why $when $who $copyright))

(defmethod help ((self options))
  (help $about)
  (let (tmp)
    (do-symbols (sym *package*) (push (list sym (symbol-name sym))  tmp))
    (dolist (pre (o $about egs))
      (loop :for (sym name) :in (sort tmp #'string< :key #'first)
            :if  (and (fboundp sym) (string-prefix-p pre name))
            :do  (format t " ~(~7a~) ~a~%" (subseq name (length pre))
                                           (documentation sym 'function))))))
 

(defmethod main((self options))  
  (loop :for (flag arg) :on (args) :by #'cdr :do
    (dolist (pre (o $about egs))
      (aif (fboundp (intern (format nil "~a~:@(~a~)" pre flag)))
           (funcall it (thing arg))))))

;;; start up
;; Every function eg-x enables a command line flag -x with one optional argument
(defun eg-h (_ &aux tmp)
  "show about help and the doco from the eg functions"
  (help our))

(defun eg-num(_)
  "test NUMs"
  (let ((num (make-num)))
    (assert (< 0.46 (div (dotimes (i 1000 num) (add num (sqrt i)))) 0.47))))

(defun eg-sym(_)
  "test SYMs"
  (let* ((sym (make-sym)))
    (dolist (char '("a" "a" "a" "a" "b" "b" "c")) (add sym char))
    (assert (< 1.37 (div sym) 1.38))))

(main our)
