<img src="https://img.shields.io/badge/tests-passing-green">
<img align=left width=300 src="/etc/img/dots4.png">
<img 
src="https://img.shields.io/badge/sbcl-2.3-orange"> <img 
src="https://img.shields.io/badge/purpose-se--ai-pink"> <img 
src="https://img.shields.io/badge/platform-osx,linux-9cf">
<h1>xfun</h1>
<p>Basic multi-objective random projections, timm@ieee.org</P> 
<p text-align=right><a href="http://tiny.cc/xfun">[home]</a> |
<a href="">[code]</a> |
<a href="">[lib]</a> |
<a href="">[tests]</a> |
<a href="">[makefile]</a></p><br clear=all>

sdsdds

dsfds

```lisp <less cli>
(defun args ()
  "access argv (for both clisp and sbcl"
  #+clisp ext:*args*  #+sbcl sb-ext:*posix-argv*)

(defun str2thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  "from string extract a number, bool, string, or '? symbol"
  (let ((it (let ((*read-eval* nil)) (read-from-string s1 ""))))
    (cond ((numberp it)     it)
          ((eq it t)        it)
          ((eq it nil)      nil)
          ((string= it "?") '?)
          (t                s1))))

(defun cli (lst &aux it)
  "replace the last item of each setting with details from CLI"
  (loop :for (key flag help b4) :in lst 
        :collect (list key flag help
                       (if (setf it (member flag (args) :test #'string=))
                           (cond ((eq b4 t)   nil)
                                 ((eq b4 nil) t)
                                 (t (str2thing (second it))))
                           b4))))
```

