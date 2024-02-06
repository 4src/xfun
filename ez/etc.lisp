(defavar *tiny* 1E-30)

;; argv
(defun args ()
  "access argv (for both clisp and sbcl"
  #+clisp ext:*args*  #+sbcl sb-ext:*posix-argv*)

;; coerce
(defun str2thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  "from string extract a number, bool, string, or '? symbol"
  (let ((it (let ((*read-eval* nil)) (read-from-string s1 ""))))
    (cond ((numberp it)     it)
          ((eq it t)        t)
          ((eq it nil)      nil)
          ((string= it "?") '?)
          (t                s1))))

;; cli
(defun cli (options &aux it)
  "CLI items that match `flag` can update items in `*settings`. For non-boolean
   settings, we expect a value after a flag. For boolean settings, flags do not
   need value (we just flip toe old value)."
  (loop :for (key flag help b4) :in options 
        :collect (list key flag help
                       (if (setf it (member flag (args) :test #'string=))
                           (cond ((eq b4 t) nil)
                                 ((eq b4 nil) t)
                                 (t (str2thing (second it))))
                           b4))))

;; macros
; ## Macros 
; Option macros
;; help
(defmacro ? (key) 
  "config items are in item four of the *options* sub-lists" 
  `(fourth (assoc ',key *options*)))

(defun print-help ()
  "to show help,  print +about+ then loop over *options*"
  (format t "~a~%~%OPTIONS:~%" +about+)
  (loop :for (_ flag help value) :in *options* :do
    (format t "    ~4a ~3a ~22a = ~a~%" flag 
      (typecase value (integer "I") (number "F") (string "S")(t ""))
      help value)))

;; Nested slot accessors.

(defmacro o (struct f &rest fs)
  (if fs `(o (slot-value ,struct ',f) ,@fs) `(slot-value ,struct ',f)))  

; Simple frequency counter.

(defmacro inca (x lst &optional (init 0))
  `(incf (cdr (or (assoc ,x ,lst :test #'equal) 
              (car (setf ,lst (cons (cons ,x ,init) ,lst)))))))

; Anaphoric if                                        ;

(defmacro aif (test-form then-form &optional else-form) 
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defun entropy (sym1)
  (with-slots (has n) sym1
    (* -1 (loop :for (_ . v) :in has :sum  (* (/ v n) (log (/ v n) 2))))))

(defmethod last-char ((s string)) (char s (1- (length s))))
(defmethod last-char ((s symbol)) (last-char (symbol-name s)))
