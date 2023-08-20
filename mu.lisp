(defvar *settings* '(
"mu: less is more"
  (seed "-s" posint "random number seed" 1234567891)
  (file "-f" file   "data file"          "../data/auto93.csv")
))

(defmacro my (x) `(fifth (assoc ',x  (cdr *settings*))))

(defmacro aif (test then &optional else) `(let ((it ,test)) (if it ,then ,else)))

(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  (let ((x (read-from-string s1)))
    (cond ((numberp x) x)
          ((eq x t)    x)
          ((eq x nil)  x)
          ((eq x '?)   x)
          (t           s1))))

(defun with-csv (file fun)
  (labels ((split (s &optional (sep #\,) (here 0))
                  (let ((there (position sep s :start here)))
                    (cons (thing (subseq s here there)) 
                          (if there (split s sep (1+ there)))))))
    (with-open-file (s (or file  *standard-input*))
      (loop (funcall fun (split (or (read-line s nil) (return))))))))

(let ((seed  1234567891)
      (n1    2147483647.0d0)
      (n2    16807.0d0)
      (b     1000000000000.0))
  (defun rand-seed (n) (setf seed n))
  (defun rand      (&optional (n 1)) (setf seed (mod (* n2  seed) n1)) (* n (- 1.0d0 (/ seed n1))))
  (defun rand-int  (&optional (n 1) &aux (base b)) (floor (* n (/ (rand base) base)))))

(defun slots (x)
  (labels ((klass-slots (y) #+clisp (clos:class-slots   (class-of y)) 
                            #+sbcl  (sb-mop:class-slots (class-of y)))
           (slot--name  (y) #+clisp (clos:slot-definition-name    y) 
                            #+sbcl  (sb-mop:slot-definition-name  y)))
    (mapcar #'slot-name (klass-slots x))))
;----------------------------------------------------------------
(defstruct col (at 0) (name ""))
(defstruct (sym (:include col)))
(defstruct (num (:include col)) (lo 1E30) (hi -1E30) (heaven 0))

(defun sym! (at name) (make-sym :at at :name name))
(defun num! (at name) (make-num :at at :name name :heaven (if (less? name) 0 1)))

(defun col! (at name)
  (funcall (if (numcol? name) #'num! #'sym!)  at name))

(defstruct cols names all x y)

(labels ((rear    (s) (elt s (1- (length s))))
         (ignore? (s) (eql #\X (rear s)))
         (numcol? (s) (upper-case? #\X (elt s 0)))
         (goal?   (s) (member (rear s) '(#\+ #\-)))
         (less?   (s) (eql (rear s) #\-)))
  (defun cols! (lst)
    (let* ((at -1)
           (i (make-cols :names lst :all (mapcar (lambda (s) (col! (incf at) s)) lst))))
      (dolist (col (reverse (cols-all i)) i)
        (unless (ignore? (col-name col))
          (if (goal? (col-name col))
            (push col (cols-y i))
            (push col (cols-x i))))))))

(defmethod add (i  x ) x)
(defmethod add ((i num) (x number))
  (setf (num-lo i) (min (num-lo i) x))
  (setf (num-hi i) (max (num-hi i) x)))
  
(defun goodbye (&optional (x 0))
  #+clisp (ext:exit x)
  #+sbcl  (sb-ext:exit :code x))
(print (slots (make-num)))
;----------------------------------------------------------
(defvar *acts* nil)

(defun act (fun flag)
  (let ((b4 (copy-tree *settings*)))
    (rand-seed (my 'seed))
    (prog1 
      (or (handler-case (funcall fun)
            (error (c) (format *error-output* "~&✋ CRASH on ~a : ~a~%" flag c)))
          (format *error-output* "~&❌ FAIL: ~a~%" flag))
      (setf *settings* b4))))

(defun acts ()
  (let ((args (coerce #+clisp ext:*args* #+sbcl sb-ext:*posix-argv* 'vector)))
    (loop for j from 0 and arg across args do
      (aif (member arg (cdr *settings*) :key #'second :test #'string=)
           (setf (fifth (car it)) (thing (elt args (1+ j)))))
      (aif (assoc arg *acts* :test #'string=)
           (act (third it) arg)))))

(defmacro defact (fun args doc &rest code)
  (let ((flag (subseq (string-downcase (symbol-name fun)) 2)))
    (push (list flag doc fun) *acts*)
    `(defun ,fun ,args ,@code)))
;----------------------------------------------------------
(defact eg--help ()
  "show help"
  (format t "~%~a~%~%OPTIONS:~%" (car *settings*))
  (loop for (_ flag ako txt __) in (cdr *settings*) do 
    (format t "  ~2a  ~6a  ~a~%" flag ako txt)) 
  (format t "~%ACTIONS:~%")
  (loop for (flag doc fun) in (reverse *acts*) do 
    (format t "  ~8a  ~a~%" flag doc))
  t)

(defact eg--fail  () "can we handle a fail?"  nil)
(defact eg--crash () "can we handle a crash?" (/ 2 0))
(defact eg--set   () "can we show settings?"  (print *settings*))

(defact eg--all () 
  "run all"                
  (+ 2 (goodbye (loop for (flag _ fun) in *acts* 
                  unless (eq fun 'eg--all) 
                  count  (not (act fun flag))))))
;----------------------------------------------------------
(acts)
