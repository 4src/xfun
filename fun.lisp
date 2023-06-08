(defvar *the* '((bins . 16) (file . "../data/auto93.csv")
                            (go . 'nothing)))

(defstruct (num (:constructor %make-num))
  (n 0) (at 0) (w 0) (txt "") (mid 0) (div 0) (m2 0) (lo 1E32) (hi -1E32))

(defun make-num (at txt rows &aux (it (%make-num :at at :txt txt)))
  (with-slots (w n most mid div m2) it
    (setf w (if (lessp txt) -1 1))
    (do-cells (at x rows it)
      (let ((d (- x mid)))
        (incf n)
        (incf mid (/ d n))
        (incf m2  (* d (- x mid)))
        (setf div  (if (< n 2) 0 (sqrt (/ m2 ( - n 1)))))))))

(defstruct (data (:constructor make-data0)) rows (cols (make-cols))
(defstruct cols names all x y klass)

(defstruct num 
  "summarizes a stream of numbers"
  (at 0) (txt "") (n 0) (w 1) ; w=1,-1 means "maximize", "minimize"
  (hi most-negative-fixnum) 
  (lo most-positive-fixnum)
  (mu 0) (m2 0))

(defstruct sym 
  "summarizes a stream of symbols"
  (at 0) (txt "") (n 0) (w 1) has (most 0) mode)

(defun make-data (rows cols &aux (self (make-data0))
  (seff (data-cols data) 
  (add (data-cols self) cols)
  (dolist (row rows data) data)
    (mapc #'add (cols-all (data-cols  data)) row)))


(defmethod add ((self cols) names)
  (with-slots (all x y klass) self
    (loop :for at :from 0 :and s :in names :do
       (let ((z    (elt s (1- (length s))))
             (what (if (uppercase-p (elt s 0)) #'make-num #'make-sym))
             (col  (funcall what :at at :txt s :w (if (eql z #\-) -1 1))))
         (push col all) 
         (unless (eql z  #\X)
           (if (eql z #\!) (setf klass col))
           (if (member z '(#\+ #\- #!)) (push col y) (push col x))))))
  self)

(defmethod add ((self sym) x)
  "update frequency counts (in `has`) and `most` and `mode`"
  (with-slots (has n mode most) self
   (unless (eql x #\?)
     (incf n)
     (incf (freq x has))
     (if (> (freq x has) most) (setf most (freq x has) mode x)))))

(defmethod add ((self num) x ) ;;; Add one thing, updating 'lo,hi'
  "updates `lo`, `hi`, `mu`, `sd`"
  (with-slots (n lo hi mu m2) self
    (unless (eq x #\?)
      (incf n)
      (let ((d (- x mu)))
        (incf mu (/ d n))
        (incf m2 (* d (- x mu)))
        (setf lo (min x lo)
              hi (max x hi))))))

  (defvar *auto93*  
  (data
    :cols  '("Cyndrs" "Vol" "Hpx" "Lbs-" "Acc+" "Model" "origin" "Mpg+")
    :rows  '((8 304.0 193 4732 18.5 70 1 10)
            (8 360 215 4615 14 70 1 10)
            (8 307 200 4376 15 70 1 10)
            (8 318 210 4382 13.5 70 1 10)
            (8 429 208 4633 11 72 1 10)
            (8 400 150 4997 14 73 1 10)
            (8 350 180 3664 11 73 1 10)
            (8 383 180 4955 11.5 71 1 10)
            (8 350 160 4456 13.5 72 1 10)
            (8 429 198 4952 11.5 73 1 10)
            (8 455 225 4951 11 73 1 10)
            (8 400 167 4906 12.5 73 1 10)
            (8 350 180 4499 12.5 73 1 10)))))
