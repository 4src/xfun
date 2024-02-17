(defun egs ()
  (labels ((eg (x) (equalp "eg-" (subseq (format nil "~(~a~)   " x) 0 3))))
    (loop :for x :being :the symbols :in *package* :if (eg (symbol-name x)) :collect x)))

(defun eg-a(x) x)
(defun eg-b(x) x)

(print (egs))
