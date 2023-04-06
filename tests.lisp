; vi: set ts=2 sw=2 sts=2 et :
(load "code")

(defun tests ()
  `((rand 
      ,(lambda (&aux (n (make-num)))
         (dotimes (i 1000) (add n (expt (rand) 2)))
         (print (mid n))
         (print (div n))
         (assert (<= .35 (mid n) .36))
         (assert (<= .30 (div n) .31))
         t))
    (num 
      ,(lambda (&aux (n (make-num)))
         (dotimes (i 1000) (add n i))
         (assert  (<= 498 (mid n) 502))))
    (sym 
      ,(lambda (&aux (s (make-sym)))
         (dolist (x '(a a a a b b c)) (add s x))
         (assert (<= 1.37 (div s) 1.38) () "sym")))
    (data 
      ,(lambda (&aux (d (src->data (? file))))
         (assert (eql 398 (length (data-rows d))))
         (assert (eql 4 (length (cols-x (data-cols d)))))))
    ))

(let ((fails 0)
      (b4    (copy-tree (setf *settings* (settings *help* (args))))))
  (if  (? help) 
    (format t "~a~%" *help*)
    (loop :for (key fun) :in (tests) :do
          (setf *settings* (copy-tree b4)
                *seed* (? seed))
          (when (member (? go) (list "all" key) 
                        :key #'string-downcase :test #'equalp)
            (format t "~%⚠️  ~a ~a " key fun)
            (cond ((funcall fun) (format t " PASSED ✅~%"))
                  (t             (format t " FAILED ❌~%")
                                 (incf fails))))))
  (goodbye fails))

