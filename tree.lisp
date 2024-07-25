#| # asdasdd

|#
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

(set-macro-character #\!  #'(lambda (s _) `(slot-value self ',(read s t nil t))))
;----------------------------------------------------------------------------------------

(defstruct (cols (:constructor %make-cols))  all x y names)
(defstruct data rows cols)


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

;; ----------------------------------------------------------------------------------------
(defmethod add ((self data) row) 
  (if !cols
    (push (add !cols row) !rows)
    (setf !cols (make-cols row))))

(defmethod adds ((self data) (file string)) 
  (with-csv file (lambda (row) (add self row))) self)

(defmethod adds ((self data) (lst cons))    
  (dolist(row lst self) (add self row)))

#|
# asdasdasdsa

dasasadada
|#
----------------------------------------------------------------------------------------
(defun make-cols (names  &aux (self (%make-cols :names names))) ; --> list[col]
  (dolist (name names self)
    (_make-cols self name (last-char name)
                (if (upper-case-p (char name 0)) #'make-num #'make-sym))))

 (defmethod _make-cols ((self cols) name z ako) ; --> nil
  (let ((col (funcall ako :txt name :pos (length !all))))
    (push col !all) 
    (when (not (eq z #\X))
      (if (member z '(#\+ #\- #\!))
          (push col !y)
          (push col !x)))))

(defmethod add ((self cols) row)
  (dolist (tmp (list !x !y) row)
    (dolist (col tmp)
      (add col (elt row (col-at col))))))

(print (make-data))
(let (rows)
  (with-csv (my-train *my*) (lambda (row) (push row rows)))
)
;----------------------------------------------------------------------------------------

