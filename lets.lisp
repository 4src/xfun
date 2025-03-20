
(defun %lets (it inner)
  "Internal helper function for the lets macro"
  (cond
    ;; Function: (name (args) body...)
    ((and (listp it) (>= (length it) 3) (symbolp (car it)) (listp (cadr it)))
     `(labels ((,(car it) ,(cadr it) ,@(cddr it))) ,inner))
    ;; Multiple-value-bin: ((vars) form)
    ((and (listp it) (listp (car it)))
     `(multiple-value-bing ,(car it) ,(cadr it) ,inner))
    ;; Regular variable:  (var value)
    (t `(let* (,it) ,inner))))

(defmacro lets (bindings &body body)
  "Combined let*, labels, and multiple-value-bind with flexible ordering"
  (let ((result `(progn ,@body)))
    (dolist (it (reverse bindings) result)
      (setf result (%lets it result)))))

;; example

(defmacro mac (expr) 
  `(pprint (macroexpand-1 ',expr)))


(mac (lets (((a b) (c 122))
            (_f1 (x) (* x 2))
            (c (_f1 21))
            ((a b) (c 122))
            (_f2 (x) (* x 2))
            (c (_f2 21)))
           (print a)))

