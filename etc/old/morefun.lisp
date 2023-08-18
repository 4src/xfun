(load "morefun-lib")
(defvar my 
  (settings 
    :about "morefun.lisp: less is more" 
    :copyright "(c) 2023 Tim Menzies <timm@ieee.org> BSD-2"
    :usage "sbcl --script morefun.lisp [OPTIONS]"
    :args ((bins  "-b"  "INT"   "how many bins"       16)
           (file  "-f"  "FILE"  "where to dead data"  "../data/auto93.lisp")
           (help  "-h"  ""      "show help"           nil)
           (min   "-m"  "REAL"  "min data size"       .5)
           (seed  "-s"  "INT"   "random number seed"  1234567891))))

(cli my)
(if (? my help) (show-help my))
