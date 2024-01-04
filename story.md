<img src="https://img.shields.io/badge/tests-passing-green"> <img
src="https://img.shields.io/badge/sbcl-2.3-orange"> <img 
src="https://img.shields.io/badge/purpose-se--ai-pink"> <img 
src="https://img.shields.io/badge/platform-osx,linux-9cf"> by
<a href="mailto:timm@ieee.org">timm@ieee.org</a>
<img align=right width=300 src="/etc/img/dots4.png">
<h1>SE and AI, just the important bits</h1>
<p>I've been working on AI and SE for oer 30 years.
Sometimes, that tech is inherently complex and hard to understand.
And often it isn't. Here I present the least code needed to
understand  the most about SE and AI that can explain itself.
  </p><br clear=all>

Useless programs are ignored and useful programs get used and updated. 
So lets write a useful program.

## Start-ing up
### Documentation and Configuration
All good programs need documentation. So lets start with a doc string
and a list of whatever config is used here.

```lisp <less about>
(defvar +about+ "
LESS: less is more
(c)2023 Tim Menzies <timm.ieee.org> BSD-2
  
USAGE:
    sbcl --script tiny.lisp [OPTIONS]
    clisp less.lisp [OPTIONS]")

(defvar *options* '(
  (BOOTSTRAPS "-b"  "number of bootstraps"                   256)
   (BOOTCONF   "-B"  "bootstrap threshold"                   .05) 
   (COHEN      "-d"  "Cohen delta"                           .35)
   (EG         "-e"  "start up actions"                "nothing")
   (FILE       "-f"  "data file"            "../data/auto93.csv")
   (HELP       "-h"  "show help"                             nil)
   (P          "-p"  "distance coeffecient"                    2)
   (SEED       "-s"  "random seed"                         10013)))
```

We need some way to access the various config settings, and some way to print that help.

```lisp <less help>
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
```

Also, optionally,  we can update `*options*`  via flags on the command-line.

```lisp <less cli>
(defun cli (lst &aux it)
  "CLI items that match `flag` can update items in `*settings`"
  (loop :for (key flag help b4) :in lst 
        :collect (list key flag help
                       (if (setf it (member flag (args) :test #'string=))
                           (cond ((eq b4 t)   nil)
                                 ((eq b4 nil) t)
                                 (t (str2thing (second it))))
                           b4))))
```

