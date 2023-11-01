(defvar *settings* ; car is help text, cdr are the settings
'("
tiny : fun with stuff
(c)2023 Tim Menzies <timm.ieee.org> BSD-2

USAGE :
sbcl --script tiny.lisp [OPTIONS] -e [ACTIONS]
"
    (bootstraps "-B"    "number of bootstraps"  256)
    (bootConf   "--CB"  "bootstrap threshold"   .05)
    (cliffs     "--CC"  "cliffs delta"          .147)
    (cohen      "-c"    "cliffs delta"          .35)
    (eg         "-e"    "start up actions"      "nothing")
    (file       "-f"    "data file"             "../data/auto93.csv")
    (help       "-h"    "show help"             nil)
    (p          "-p"    "distance coefficient"  2)
    (seed       "-s"    "random seed"           1234567891)
    ))


