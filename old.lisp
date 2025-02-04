(defun s->fn (s &optional (pre ""))
  (intern (string-upcase (format nil "~a~a" pre s))))

(defmacro with-constructor (&rest all)
  `(progn 
     . ,(loop :for (_ (x . meta) . slots) :in all :collect
          `(defstruct (,x (:constructor ,(s->fn it "%MAKE-"))
                          (:print-function (lambda (p s _) (_print p s ',x ',slots ))) 
                          ,meta) . ,slots))))

(defun _print (self str klass slots)
  (labels ((hdr (x) (if (consp x) (car x) x))
           (ok  (x) (not (eql #\_ (char (symbol-name x) 0)))))
           (val (x) (if (ok x) (cons (hdr x) (slot-value self (hdr x)))))
    (format str "(~a ~{~a~})" ',klass (remove-if #'null (mapcar #'val slots)))))
