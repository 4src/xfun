(defstruct about
  (what      "core.lisp")
  (why       "find independent values that select for best dependent values")
  (when      2024)
  (who       "Tim Menzies")
  (copyright "BSD-2")
  (act       '(act eg)))   ; go-x command are available on the command line as -x [arg]

(defstruct my
  (seed  10013)
  (train "data/misc/auto93.csv")
  (about (make-about)))

(defvar *my* (make-my))
;----------------------------------------------------------------------------------------

(defstruct (cols (:constructor %make-cols))  all x y names)
(defstruct data rows cols)

(set-macro-character #\$  #'(lambda (s _) `(slot-value i ',(read s t nil t))))

;----------------------------------------------------------------------------------------
(defmethod adds ((self data) (file string)) 
  (with-csv file (lambda (row) (add data row))) data)

(defmethod adds ((self data) (lst cons))    
  (dolist(row file data) (add data row)))

(defmethod add ((self data) row) 
  (if (data-cols data) 
    (push (add (data-cols data) row) (data-rows data))
    (setf (data-cols self) (make-cols row))))

;----------------------------------------------------------------------------------------
(defun make-cols (names  &aux (self (%make-cols :names names))) ; --> list[col]
  (dolist (name names self)
    (_make-cols self name (last-char name)
                (if (upper-case-p (char name 0)) #'make-num #'make-sym))))

 (defmethod _make-cols ((self cols) name z ako) ; --> nil
  (let ((col (funcall ako :txt name :pos (length (cols-all self)))))
    (push col (cols-all self))
    (when (not (eq z #\X))
      (if (member z '(#\+ #\- #\!))
          (push col (cols-y self))
          (push col (cols-x self))))))

(defun add ((self cols) row)
  (dolist (cols (list (cols-x cols) (cols-y cols)) row)
    (dolist (col cols)
      (add col (elt row (cols-at col))))))

(print (make-data))
(let (rows)
  (with-csv (my-train *my*) (lambda (row) (push row rows)))
)
;----------------------------------------------------------------------------------------
(defun last-char (s) ; --> char
  (if (symbolp s)
      (last-char (symbol-name s))
      (char s (1- (length s)))))

(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s))) ; --> atom
  (let ((it (let ((*read-eval* nil))
              (read-from-string s1 ""))))
    (cond ((numberp it)     it)
          ((eq it t)        t)
          ((eq it nil)      nil)
          ((string= it "?") '?)
          (t                s1))))

(defun string2things (s &optional (sep #\,) (here 0)) ; --> list
  (let ((there (position sep s :start here)))
    (cons (thing (subseq s here there))
          (if there (string2things s sep (1+ there))))))

(defun with-csv (&optional file (fun #'print) (filter #'string2things)) ; --> nil
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))


