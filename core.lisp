; vim : set ts=2 sw=2 sts=2
(set-macro-character #\$ #'(lambda (s _)
                             (declare (ignore _))
                             `(slot-value self ',(read s t nil t))))

(defmacro o (struct f &rest fs)
  (if fs `(o (slot-value ,struct ',f) ,@fs) `(slot-value ,struct ',f)))

(defmacro aif (test yes &optional no) 
  `(let ((it ,test)) (if it ,yes ,no)))

(defmacro has (x lst &optional (init 0))
  `(cdr (or (assoc ,x ,lst :test #'equal) 
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))
;---------- --------- --------- --------- --------- --------- --------- ---------
(defstruct stats (boostraps 1234567891) (cohen 0.35))

(defstruct options (stats (make-stats)))

(defvar our (make-options))
;---------- --------- --------- --------- --------- --------- --------- ---------
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
    (loop (funcall fun (funcall filter (or (read-line s nil) (return file)))))))
;---------- --------- --------- --------- --------- --------- --------- ---------
(defstruct sym (pos 0) (txt " ") (n 0)  seen mode (most 0))
(defstruct num (pos 0) (txt " ") (n 0) (lo 1e30) (hi -1e30) (mu 0) (m2 0) (goal 1))

(defun makeNum (&key txt (pos 0))
  (make-num :pos pos :txt txt :goal (if (eq #\- (last-char txt)) 0 1)))

(defmethod add ((self num) x)
  (unless (eq #\? x)
    (incf $n)
    (let ((d (- x $mu)))
      (incf $mu  (/ d $n))
      (incf $m2  (* d (-  x $mu)))
      (setf $lo  (min x $lo)
            $hi  (max x $hi)))))

(defmethod add ((self sym) x)
  (unless (eq x '?)
    (incf $n)
    (let ((new (incf (has x $seen))))
      (if (> new $most)
          (setf $mode x 
                $most new)))))

(defmethod mid ((self num)) $mu)
(defmethod mid ((self sym)) $mode)

(defmethod div ((self num)) (if (< $n 2) 0 (sqrt (/ $m2 (- $n 1)))))
(defmethod div ((self sym))
  (* -1 (loop :for (_ . v) :in $has :sum  (* (/ v $n) (log (/ v $n) 2)))))
;---------- --------- --------- --------- --------- --------- --------- ---------
(defstruct data rows cols)
(defstruct cols all x y names)

(defmethod clone ((self data) &optional inits)
  (slurp (make-data) (cons (data-cols (cols-name self)) inits)))
  
(defmethod slurp ((self data) (file string))
  (with-csv file (lambda (row) (add self row))))

(defmethod slurp ((self data) (lst cons))
    (dolist (row lst self) (add self row)))

(defmethod add ((self data) row)
  (if cols 
      (push (add cols row) $rows) 
      (setf $cols (make-cols (row-cells row1)))))
      
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

;---------- --------- --------- --------- --------- --------- --------- ---------
(defun egs()
  (labels ((down (x) (string-downcase (symbol-name x)))
           (eg   (s) (equalp "eg-" (subseq s 0 (min 3 (length s))))))
    (loop :for x :being :the symbols :in *package* :if (eg (down x)) :collect x)))

(defun eg-o () (o our stats cohen))
