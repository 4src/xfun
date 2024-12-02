#+sbcl
(declaim (sb-ext:muffle-conditions cl:style-warning))

(defstruct about 
  "Struct for file meta info."
  (what  "mink.lisp")
  (why   "optimization via recursive kmeans")
  (when  "(c) 2024")
  (how   "MIT license")
  (who   "Tim Menzies")
  (where "timm@ieee.org"))

(defstruct bayes
 (m 1)
 (k 2))

(defstruct settings 
  "Struct for all settings."
  (seed    1234567891)
  (buckets 2)
  (pp      2)
  (train   "data/auto93.lisp")
  (about  (make-about))
  (bayes  (make-bayes)))

(defvar *settings* (make-settings))

;--------------------------------------------------------------------
(defstruct (col)
  "COLs are NUMs and SYMS have `name`; are found `at` some column index."
  (n 0) (at 0) (name " "))

(defstruct (sym (:include col) (:constructor %make-sym))
  "SYMs track a `count` of symbols. The `mode` is the `most` common symbol."
  count mode (most 0))

(defstruct (num (:include col) (:constructor %make-num))
  "NUMs tracks `lo`, `hi`,  mean `mu`, stdev `sd` seen so far"
  (goal 1) (mu 0) (m2 0) (sd 0) (lo 1E32) (hi -1E32))

(defstruct (cols (:constructor %make-cols))
  "COLS are factories turning  strings to NUMs or COLs."
  names all x y)

(defstruct (data (:constructor %make-data))
  "'rows' are summazied in 'cols'."
  rows cols)

;--------------------------------------------------------------------
(defmacro o (x f &rest fs) 
  "nested slot access"
  (if fs `(o (slot-value ,x ',f) . ,fs) `(slot-value ,x ',f)))

(set-macro-character #\$ #'(lambda (s _) `(slot-value self ',(read s t nil t))))

(defmacro ? (&rest slots) 
  `(o *settings* . ,slots))

;--------------------------------------------------------------------
(defun make-data (&optional src &aux (self (%make-data)))
  (labels ((fun (row) (add self row)))
    (if (stringp src) (mapfile #'fun src) (mapcar #'fun src)))
  self)

(defun make-num (&key (at 0) (name " ") &aux (self (%make-num :at at :name name)))
  (setf $goal (if (eql #\- (last-char $name)) 0 1))
  self)

(defun make-cols (names &aux (self (%make-cols :names names)))
  (dolist (name names self) 
    (let* ((a   (char name 0)) 
           (z   (last-char name))
           (fun (if (upper-case-p a) #'make-num #'make-sym))
           (col (funcall fun :name name :at (length $all))))
      (push col $all)
      (unless (eql z #\X) 
        (if (member z (list #\! #\- #\+)) (push col $y) (push col $x))))))

;--------------------------------------------------------------------
(defmethod add ((self data) (row cons))
  "First time, create columns. Next, summarize `row` in `cols` and store in `rows`."
  (if $cols
    (push (add $cols row) $rows)
    (setf $cols (make-cols row))))

(defmethod add ((self cols) row)
  "Summarise a row in the `x` and `y` columns."
  (dolist (lst (list $x $y) row)
    (dolist (col lst)
      (add col (at col row)))))

(defmethod add ((self col) x)
  "Summarize `x` in a column (unless it is don't know."
  (unless (eq x '?)
     (incf $n)
     (add1 self x)))

(defmethod add1 ((self num) (x number)) 
  "Update a NUM."
  (let ((d (- x $mu)))
    (incf $mu (/ d $n))
    (incf $m2 (* d (-  x $mu)))
    (setf $lo (min x $lo)
          $hi (max x $hi)
          $sd (if (< $n 2) 0 (sqrt (/ $m2 (- $n 1)))))))

(defmethod add1 ((self sym) x) 
  "Update a SYM."
  (let ((new 
          (incf (cdr (or (assoc x $count :test #'equal) 
                         (car (setf $count (cons (cons x 0) $count))))))))
    (if (> new $most)
      (setf $mode x
            $most new))))

;--------------------------------------------------------------------
(defmethod at ((self col) row) (elt row $at))

(defmethod mid ((self num)) $mu)   
(defmethod mid ((self sym)) $mode) 

(defmethod div ((self num)) ; --> float
  "NUMbers have standard deviation"
  (if (< $n 2) 0 (sqrt (/ $m2 (- $n 1)))))

(defmethod div ((self sym)) ; --> float
  "SYMbols have entropy"
  (* -1 (loop :for (_ . v) :in $has :sum (* (/ v $n) (log (/ v $n) 2)))))

;--------------------------------------------------------------------
(defmethod like ((self data) row &key nall nh) ; --> float
  "Return the log likelihood of a row."
  (labels ((num (n) (if (< n 0) 0 (log n))))
    (let* ((prior (/ (+ (length $rows) (? bayes k)) 
                     (+ nall (* nh (? bayes k))))))
      (+ (num prior) (loop :for col :in (o $cols x) 
                           :if (not (equal '? (at col row)))
                           :sum (num (like col (at col row) :prior prior)))))))

(defmethod like ((self sym) x &key prior) ; --> float
  "Return likelhood of a SYMbol."
  (/ (+ (count $seen x) (* (? bayes m) prior)) 
     (+ $n (? bayes m))))

(defmethod like ((self num) x &key prior) ; --> float
  "Return likelhood of a NUMber."
  (let ((sd (+ $sd 1E-30)))
    (/ (exp (- (/ (expt (- x $mu) 2) (* 2 (expt sd 2)))))
       (* sd (sqrt (* 2 pi))))))
