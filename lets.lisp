(defmacro lets (bindings &body body)
  "Combined let*, labels, and multiple-value-bind with flexible ordering"
  (let ((out `(progn ,@body)))
    (dolist (b (reverse bindings) out)
      (setf out (cond ((and (listp b) (> (length b) 2) (symbolp (car b)) (listp (cadr b)))
                       `(labels ((,(car b) ,(cadr b) ,@(cddr b))) ,out))
                      ((and (listp b) (listp (car b)))
                       `(multiple-value-bing ,(car b) ,(cadr b) ,out))
                      (t `(let* (,b) ,out)))))))

;; example
(pprint (macroexpand-1 
          '(lets (((a b) (c 122))
                  (_f1 (x) (* x 2))
                  (c (_f1 21))
                  ((a b) (c 122))
                  (_f2 (x) (* x 2))
                  (c (_f2 21)))
                 (print a))))
