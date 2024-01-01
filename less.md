<img src="https://img.shields.io/badge/tests-passing-green"> <img
src="https://img.shields.io/badge/sbcl-2.3-orange"> <img 
src="https://img.shields.io/badge/purpose-se--ai-pink"> <img 
src="https://img.shields.io/badge/platform-osx,linux-9cf"> by
<a href="mailto:timm@ieee.org">timm@ieee.org</a>
<img align=left width=250 src="/etc/img/dots4.png">
<h1>SE and AI, just the important bits</h1>
<p>101 simple SE tricks for simpler AI.</P><br clear=all>

<h2>The Great Secret</h2>

What have we learned?


dsfds

```lisp <less cli>
(defun args ()
  "access argv (for both clisp and sbcl"
  #+clisp ext:*args*  #+sbcl sb-ext:*posix-argv*)

(defun str2thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  "from string extract a number, bool, string, or '? symbol"
  (let ((it (let ((*read-eval* nil)) (read-from-string s1 ""))))
    (cond ((numberp it)     it)
          ((eq it t)        t)
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

