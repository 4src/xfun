(defun egs ()
  (labels ((eg (x) (equal "eg-" (subseq (format nil "~(~a~)   " (symbol-name x)) 0 3))))
    (loop :for x :being :the symbols :in *package* :if (eg x) :collect x)))

(defun eg-a(x) x)
(defun eg-b(x) x)

(defun it(a) a)
(print (egs))
