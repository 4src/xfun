(defun eg-h () (format t "
slope.lisp: cinnrecemtanl stochastic optimisation
(c) 2025 TIm Menzies <timm@ieee.org> MIT Liencse~%~%"))

#+sbcl (declaim (sb-ext:muffle-conditions cl:style-warning))

(defun reads-from-string (s &optional (sep #\,) (here 0))
  (let ((there (position sep s :start here)))
    (cons (read-from-string (subseq s here there))
          (if there (reads-from-string s sep (1+ there))))))

(defun with-csv (&optional file (fun #'print) end)
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (reads-from-string (or (read-line s nil) (return end)))))))

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
