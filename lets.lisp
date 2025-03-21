;Here's one I've dreamt of  for a while: a LISP variant of "let" that handles let*, labels, multiple-value-bind. Built with http://perplexiity.ai. FYI it required much wrangling and testing, but the key expansion tactic seen  at end of "lets" was all its idea (impressive).
(defmacro lets (bindings &body body)
  (labels ((fun (x b4)
             (cond
               ((and (listp x) (eq (car x) 'lots))
                (reduce #'fun (cdr x) :initial-value b4 :from-end t))
               ((and (listp x) (> (length x) 2) (symbolp (car x)) (listp (cadr x)))
                `(labels ((,(car x) ,(cadr x) ,@(cddr x))) ,b4))
               ((and (listp x) (listp (car x)))
                `(destructuring-bind ,(car x) ,(cadr x) ,b4))
               (t `(let* (,x) ,b4)))))
    (reduce #'fun bindings :initial-value `(progn ,@body) :from-end t)))

; (defmacro lets (bindings &body body)
;  "Combined let*, labels, and multiple-value-bind with flexible ordering"
;  (labels ((fun (x b4)
;                (cond ((and (listp x) (> (length x) 2) (symbolp (car x)) (listp (cadr x)))
;                       `(labels ((,(car x) ,(cadr x) ,@(cddr x))) ,b4))
;                      ((and (listp x) (listp (car x)))
;                       `(multiple-value-bind ,(car x) ,(cadr x) ,b4))
;                      (t `(let* (,x) ,b4)))))
;    (reduce #'fun bindings :initial-value `(progn ,@body) :from-end t)))
;


(pprint (macroexpand-1 
          '(lets (((a b) (a 1 2))
                  (f (x) (* x 2))
                  (~ (a . c) '(1 . 2)
                  (x 10))
                  (list a b c d (f x)))))


;Got any other cool  LISP macros ?  My one  fav is to make "self" the first defmethod arg then use   

(set-macro-character #\$  
   #'(lambda (s _) `(slot-value self ',(read s t nil t))))  

;so self's slots can be accessed/changed using just $slot
