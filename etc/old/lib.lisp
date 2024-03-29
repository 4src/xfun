; vi: set ts=2 sw=2 sts=2 et :
;## Globals
(defvar *settings* nil)

;## Macros
(defmacro ? (x &optional (lst '*settings*)) 
  "alist accessor, defaults to searching `*settings*`"
  `(cdr (assoc ',x ,lst :test #'equalp)))

(defmacro aif (test then &optional else)      
  "used to test on a result that is also needed by `then`"
  `(let ((it ,test)) (if it ,then ,else)))

(defmacro freq (x lst &optional (init 0))      
  "frequency counts for small group of symbols (say, less than 50)"
  `(cdr (or (assoc ,x ,lst :test #'equal) 
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))
;-------------------------------------------------------------------------------
;## Sys
(defun args () 
  "accessing command-line flats"
  #+clisp ext:*args*  
  #+sbcl sb-ext:*posix-argv*)

(defun goodbye (&optional (x 0)) 
  "quit list"
  #+clisp (ext:exit x)
  #+sbcl  (sb-ext:exit :code x))
;-------------------------------------------------------------------------------
(defvar *seed* 10013)
(defun rand (&optional (n 1))
  "random float 0.. < n"
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 1) &aux (base 10000000000.0))
  "random int 0..n-1"
  (floor (* n (/ (rand base) base))))

(defun round2 (number &optional (digits 2))
  "round to `digits` number of decimal places"
  (let* ((div (expt 10 digits))
         (tmp (/ (round (* number div)) div)))
    (if (zerop digits) (floor tmp) (float tmp)))
;-------------------------------------------------------------------------------
;##  Lists
(defun per (seq &optional (p .5))
  (elt seq (floor (* (min .999999 (max 0 p)) (length seq)))))
;-------------------------------------------------------------------------------
;## Strings
(defun trim (s) 
  "kill whitespace at start, at end"
  (string-trim '(#\Space #\Tab #\Newline) s))

(defun got (s n &rest chars)
  "Does `s` hold any of `chars` at position `n` (negative `n` means 'from end of string')"
  (let ((n (if (>= n 0) n (+ (length s) n))))
    (if (and (stringp s)  (>= (1- (length s)) n))
      (dolist (c chars)
        (if (eql c (char s n))
          (return-from got t))))))

(defun split (s &optional (sep #\,) (filter #'thing) (here 0))
  "split  `s`, divided by `sep` filtered through `filter`"
  (let* ((there (position sep s :start here))
         (word  (funcall filter (subseq s here there))))
    (labels ((tail () (if there (split s sep filter (1+ there)))))
      (if (equal word "") (tail) (cons word (tail))))))

(defun words (s) 
  "divide a string on space"
  (split s #\Space #'trim))
;-------------------------------------------------------------------------------
;## Strings to Things
(defun thing (s &aux (s1 (trim s)))
  "coerce `s` into a number or string or t or nil or #\?"
  (cond ((equal s1 "?") #\?)
        ((equal s1 "t") t)
        ((equal s1 "nil") nil)
        (t (let ((n (read-from-string s1 nil nil))) 
             (if (numberp n) n s1)))))

(defun with-file (file fun &optional (filter #'split))
  "call `fun` for each line in `file`"
  (with-open-file (s file) 
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))
;-------------------------------------------------------------------------------
;## Settings
(defun settings (s &optional args)
  "for lines like '  -Key Flag ..... Default', return `(KEY . DEFAULT)`"
  (loop 
    :for (flag key . lst) 
    :in  (split s #\NewLine #'words)
    :if  (got flag 0 #\-) 
    :collect (cons (intern (string-upcase key)) 
                   (cli args flag (thing (car (last lst)))))))

(defun cli (lst flag b4)
  "if `flag` in `lst`, then update `b4` from `lst`"
  (aif (member flag lst :test #'equal)
    (cond ((eql b4 t)   nil)
          ((eql b4 nil) t)
          (t            (thing (second it))))
    b4))
