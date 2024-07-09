; vim : set ts=2 sw=2 sts=2
(defun make ()
  #+sbcl (setf sb-ext:*muffled-warnings* 'style-warning)
  (load "core"))

;---------- --------- --------- --------- --------- --------- --------- ---------
(set-macro-character #\$ #'(lambda (s _) `(slot-value self ',(read s t nil t))))

(defmacro o (struct f &rest fs)
  (if fs `(o (slot-value ,struct ',f) ,@fs) `(slot-value ,struct ',f)))

(defmacro aif (test yes &optional no) 
  `(let ((it ,test)) (if it ,yes ,no)))

(defmacro has (lst x &optional (init 0))
  `(cdr (or (assoc ,x ,lst :test #'equal) 
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

;---------- --------- --------- --------- --------- --------- --------- ---------
(defstruct data rows cols)

(defstruct (cols (:constructor %make-cols)) all x y names)

(defstruct sym (pos 0) (txt " ") (n 0)  seen mode (most 0))

(defstruct (num (:constructor %make-num))
               (pos 0) (txt " ") (n 0) (lo 1e30) (hi -1e30) (mu 0) (m2 0) (goal 1))

(defstruct stats   (boostraps 1234567891) (cohen 0.35))
(defstruct options (stats (make-stats)))

(defvar our (make-options))

;---------- --------- --------- --------- --------- --------- --------- ---------
; nums and syms
(defun make-col (&key pos txt)
  (funcall (if (upper-case-p (char txt 0)) #'make-num #'make-sym) :pos pos :txt txt))

(defun make-num (&key txt (pos 0))
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
  (* -1 (loop :for (_ . v) :in $has :sum  (* (/ v $n) (log (/ v $n) 2)))))

;---------- --------- --------- --------- --------- --------- --------- ---------
; data and cols
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

(defun make-cols (names)   
  (let (all x y (n -1))
    (dolist (col (loop :for s :in names :collect (make-col :pos (incf n) :txt s)))
      (push col all)
      (when (not (eq #\X (last-char (o col txt))))
        (if (member (last-char (o col txt)) '(#\+ #\-))
            (push col y)
            (push col x))))
    (%make-cols :names names :all all :x x :y y)))

(defmethod add ((self cols) row)
  (dolist (cs (list $x $y) row)
    (dolist (col cs)
      (add col (elt row $pos)))))

;---------- --------- --------- --------- --------- --------- --------- ---------
; misc
(defmethod last-char ((s string)) (char s (1- (length s))))
(defmethod last-char ((s symbol)) (last-char (symbol-name s)))

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

(defun with-csv (file &optional (fun #'print) (filter #'split))
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))

;---------- --------- --------- --------- --------- --------- --------- ---------
; start up
(defun egs()
  (labels ((down (x) (string-downcase (symbol-name x)))
           (eg   (s) (equalp "eg-" (subseq s 0 (min 3 (length s))))))
    (loop :for x :being :the symbols :in *package* :if (eg (down x)) :collect x)))

(defun eg-o () (o our stats cohen))
