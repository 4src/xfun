#!/usr/bin/env sbcl --script
; vim: ts=2 sw=2 et :

(defpackage :spy (:use :cl))
(in-package :spy)

(defvar *help* "
spy.lisp: sequential model optimization
(c) 2024 Tim Menzies <timm@ieee.org> BSD-2")

(defvar  *options* '(
  ;tag   cliFlag  help text            default
  ;---   -------  ---------            -------
  (k     "-k"     "kth value"          2)
  (file  "-f"     "csv data file"      "data/auto93.lisp")
  (goal  "-g"     "start-up action"    one)
  (seed  "-s"     "random number seed" 1234567891)
  (top   "-t"     "keep top items"     0.8)
  (start "-n"     "initial budget"     4)
  (stop  "-N"     "total budget"       20)
  (help  "-h"     "show help"          nil)))

; ---------------------------------------------------------------------------------------
(set-macro-character #\$ #'(lambda (s _) `(slot-value self ',(read s t nil t))))
(set-macro-character #\! #'(lambda (s _) `(fourth (assoc ',(read s t nil t) *options*))))

(defmacro has (x lst)  `(cdr (or (assoc ,x ,lst :test #'equal)
                                 (car (setf ,lst (cons (cons ,x 0) ,lst))))))

; ---------------------------------------------------------------------------------------
(defstruct (sym (:constructor %sym)) (n 0) (at 0) (txt " ") seen (most 0) mode)

(defstruct (num (:constructor %num)) 
  (n 0) (at 0) (txt " ") (mu 0) (m2 0) (lo 1E30) (hi -1E30) (want 0))

(defstruct (cols (:constructor %cols)) (ncols -1) x y all names klass)

(defstruct (data (:constructor %data)) rows cols (fun #(lambda (&rest _) _)))

; ----------------------------------------------------------------------------------------
(defun make-sym (&optional (s " ") (n 0)  &aux (self (%sym :txt s :at n))) self)

(defun make-num (&optional (s " ")  (n 0) &aux (self (%num :txt s :at n)))
  (setf $want (if (end $txt #\-) 0 1))
  self)

(defun make-cols (names &aux (self (%cols :names names)))
  (dolist (s names)
    (incf $ncols)
    (let ((col (if (upper-case-p (char s 0)) (make-num s $ncols) (make-sym s $ncols))))
      (push col $all)
      (unless (end s #\X)
        (if   (end s #\!)         (setf $klass col))
        (if   (end s #\- #\+ #\!) (push col $y) (push col $x)))))
  (setf $all (reverse $all))
  self)

(defun make-data (src &key fun rankp &aux (self (%data :fun fun)))
  (if (stringp src)
    (csv src (lambda (row) (add self row)))
    (dolist (row src) (add self row)))
  (if rankp (setf $rows (sort $rows #'< :key (lambda (row) (d2h self row)))))
  self)

(defmethod clone ((self data) &optional inits rankp)
  (make-data (cons (cols-names $cols) inits) :rankp rankp))

; ---------------------------------------------------------------------------------------
(defmethod add ((self sym) x)
  (unless (eq x '?) 
    (incf $n)
    (let ((new (incf (has x $seen))))
      (if (> new $most)
        (setf $mode x 
              $most new)))))

(defmethod add ((self num) x)
  (unless (eq x '?)
    (incf $n)
    (let ((d (- x $mu)))
      (incf $mu (/ d $n))
      (incf $m2 (* d (-  x $mu)))
      (setf $lo (min x $lo)
            $hi (max x $hi)))))

(defmethod add ((self cols) lst) 
  (mapcar (lambda (col x) (add col x) x) $all lst)
  lst)

(defmethod add ((self data) row)
  (if $cols
    (push (add $cols row) $rows)
    (setf $cols (make-cols row))))

; ---------------------------------------------------------------------------------------
(defmethod mid ((self num)) (num-mu self))
(defmethod mid ((self sym)) (sym-mode self))

(defmethod div ((self num)) (sqrt (/ $m2  (- $n 1))))
(defmethod div ((self sym)) 
  (* -1 (loop :for (_ . v) :in $seen :sum  (* (/ v $n) (log (/ v $n) 2)))))

; ---------------------------------------------------------------------------------------
(defmethod like ((self sym) x &key prior)
  (/ (+ (or (cdr (assoc x $seen)) 0) 
        (* !m prior)) 
     (+ $n !m)))

(defmethod like ((self num) x &key prior)
  (let ((sd (+ (div self) 1E-30)))
    (/ (exp (- (/ (expt (- x $mu) 2) (* 2 (expt sd 2)))))
       (* sd (sqrt (* 2 pi))))))

(defmethod like ((self data) row &key nall nh)
  (let* ((prior (/ (+ (length $rows) !k) 
                   (+ nall (* nh !k)))))
    (+ (log prior) (loop :for col :in (cols-x $cols) :sum (_loglike row col prior)))))

(defun _loglike (row col prior &aux (out 0) (x (elt row (col-at col))))
  (unless (eql x '?)
    (let ((inc (like col x :prior prior)))
      (unless (zerop inc) 
        (setf out (log inc)))))
  out)

; ---------------------------------------------------------------------------------------
(defmethod d2h ((self num)  lst) (abs (- $want (norm self (elt lst $at)))))

(defmethod d2h ((self data) lst)
  (let* ((ys (cols-y (data-cols self)))
         (d  (loop :for col :in ys :sum (expt (d2h col lst) 2))))
    (expt (/ d (length ys)) 0.5)))

; ---------------------------------------------------------------------------------------
(defmethod smo ((self data) &optional (gt (lambda (b r) (- b r))))
  (labels ((likes (r best rest n)
                  (funcall gt (like best r :nall n :nh 2) 
                              (like rest r :nall n :nh 2)))
           (guess-and-cut (todo sorted-labelled-rows)
               (let* ((n    (length sorted-labelled-rows))
                      (cut  (floor (expt n !best)))
                      (top  (floor (* n !top)))
                      (best (clone self (subseq sorted-labelled-rows 0 cut)))
                      (rest (clone self (subseq sorted-labelled-rows cut))))
                 (subseq (sort todo #'> :key (lambda (r) (likes r best rest n))) 0 top)))
           (smo1 (budget todo done) 
                 (let ((sorted-labelled-rows (data-rows (clone self done t))))
                   (if (or (< budget 1) (<= (length todo) 3))
                     (first sorted-labelled-rows)
                     (let ((todo (guess-and-cut todo sorted-labelled-rows)))
                       (push (pop todo) done)
                       (smo1 (1- budget) todo done))))))
    (nshuffle $rows)
    (smo1 (- !stop !start) (subseq $rows !start) 
                           (subseq $rows 0 !start))))

; ---------------------------------------------------------------------------------------
(defun end   (s &rest lst) (member (char s (1- (length s))) lst))
(defun adds  (col1 lst)    (dolist (x lst col1) (add col1 x)))
(defun slurp (file)        (with-open-file (s file) (read s)))

(defun args ()     #+clisp ext:*args*   #+sbcl sb-ext:*posix-argv*)
(defun goodbye (x) #+clisp (ext:exit x) #+sbcl (sb-ext:exit :code x))

(defun normal (&optional (mu 0) (sd 1))
   (+ mu (* sd (sqrt (* -2 (log (rand)))) (cos (* 2 pi (rand))))))

(defun str2thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  (let ((*read-eval* nil)) (read-from-string s1 "")))

(defun cli (options &aux it)
  (loop :for (key flag help b4) :in options :collect ; maybe swap b4 for a new value
        (list key flag help (if (setf it (member flag (args) :test #'string=))
                              (cond ((eq b4 t) nil)
                                    ((eq b4 nil) t)
                                    (t (str2thing (second it))))
                              b4))))

(defun csv (file &optional (fun #'print))
  (with-open-file (str file)
    (loop (funcall fun (str2thing (or (read-line str nil) (return-from csv)))))))

(defun print-help ()
  (format t "~a~%~%OPTIONS:~%" *help*)
  (loop :for (_ flg txt is) :in *options* 
        :do (format t "  ~4a ~20a = ~a~%" flg txt is)))

(defvar *seed* 10013)
(defun rint (&optional (n 1) &aux (base 1E10)) (floor (* n (/ (rand base) base))))
(defun rand (&optional (n 1))
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
    do (rotatef (elt sequence (rint i))
                (elt sequence (1- i))))
  sequence)

; ---------------------------------------------------------------------------------------
(defvar *egs* nil)

(defmacro eg (tag &rest code) `(push (list ',tag (lambda () ,@code)) *egs*))

(defun run (flag fun &aux (b4 (copy-tree *options*)))
  (setf *seed* !seed)
  (let ((passed (funcall fun)))
    (setf *options* (copy-tree b4))
    (unless passed (format t "❌ FAIL: ~(~a~)~%" flag))
    passed))

(defun main (&optional update)
  (labels ((ok   (x) (member !goal (list 'all x))))
    (if update (setf *options* (cli *options*)))
    (if !help
      (print-help)
      (goodbye  (loop :for (flag fun) :in (reverse *egs*) 
                      :if (ok flag) :count (not (run flag fun)))))))  

; ---------------------------------------------------------------------------------------
(eg one (print *options*))

(eg norms (let ((n (make-num)))
            (dotimes (i 100000) (add n (normal 20 2)))
            (format t "~,3f ~,3f~%" (mid n) (div n))
            t))

(eg rand (let ((n (make-num)))
           (dotimes (i 1000) (add n (rand)))
           (format t "~,3f~%" (mid n))
           t))

(eg sym (let ((n (adds (make-sym) '(a a a a b b c))))
           (format t "~,3f~%" (mid n))
           t))

(eg csv (csv !file) t)

(eg cols 
    (print
      (make-cols '("Clndrs"  "Volume"  "HpX" "Lbs-" "Acc+" "Model" "origin" "Mpg+")))
    t)

(eg data 
    (let ((d (make-data !file)))
      (print (cols-x (data-cols d)))))

; ---------------------------------------------------------------------------------------
(main t) 
