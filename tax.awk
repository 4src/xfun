#!/usr/bin/env gawk -f
/^;;/     { if (c) print ""; c=0; print substr($0,3); next }
/^$/      { next }
/^[[(#]/  { if (!c) print ""; c=1 }
          { print "    " $0 }
