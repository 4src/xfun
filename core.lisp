; vim : set ts=2 sw=2 sts=2
(set-macro-character #\$ #'(lambda (s _) `(slot-value self ',(read s t nil t))))
(set-macro-character #\! #'(lambda (s _) `(o the ,(read s t nil t))))

(defmacro o (struct f &rest fs)
  (if fs `(o (slot-value ,struct ',f) ,@fs) `(slot-value ,struct ',f)))

(defmacro aif (test yes &optional no) 
  `(let ((it ,test)) (if it ,yes ,no)))

(defmacro inca (x lst &optional (init 0))
  `(incf (cdr (or (assoc ,x ,lst :test #'equal) 
                  (car (setf ,lst (cons (cons ,x ,init) ,lst)))))))
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
  (with-open-file (s (or file  *standard-input*))
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))
;---------- --------- --------- --------- --------- --------- --------- ---------
(defstruct sym (pos 0) (txt " ") (n 0)  has mode (most 0))
(defstruct num (pos 0) (txt " ") (n 0) (lo 1e30) (hi -1e30) (mu 0) (m2 0) (goal 1))

(defun makeNum (&key (at 0) (txt " "))
  (make-num :at at :txt txt  :goal (if (eq #\- (last-char txt)) 0 1)))
;---------- --------- --------- --------- --------- --------- --------- ---------
(defun egs()
  (labels ((down (x) (string-downcase (symbol-name x)))
           (eg (s) (equalp "eg-" (subseq s 0 (min 3 (length s))))))
    (loop :for x :being :the symbols :in *package* :if (eg (down x)) :collect x)))

(defun eg-o () (o our stats cohen))
