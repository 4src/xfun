(defun eg-h () (format t "
slope.lisp: incremental  stochastic optimisation
(c) 2025 TIm Menzies <timm@ieee.org> MIT Liencse~%~%"))

#+sbcl (declaim (sb-ext:muffle-conditions cl:style-warning))

(defun slurp (s &optional rawp) 
  (if rawp s (read-from-string s)))

(defun splits (s &optional rawp (sep #\,) (here 0))
  (let ((there (position sep s :start here)))
    (cons (slurp (subseq s here there) rawp)
          (if there (splits s rawp sep (1+ there))))))

(defun with-csv (&optional file (fun #'print) end (n -1))
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (splits (or (read-line s nil) (return end)) 
                               (zerop (incf n)))))))

(defun eg--csv()
   (with-csv "../data/auto93.csv"))

(defun args ()
  (cdr #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*))

(defun main ()
 (loop :for (flag arg) :on (args) :by #'cdr 
  :do  (let ((com (intern (format nil "EG~:@(~a~)" flag))))
         (when (fboundp com)
           (if arg
              (funcall com (as arg))
              (funcall com))))))


(main)
