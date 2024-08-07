function com(s) { return s ~ /^; ?/ }

BEGIN                 { STOP="" }
NR < 2                { next } 
com($0) && !com(last) { printf STOP }
!com($0) && com(last) { print "\n```lisp"; STOP="```\n\n" }
END                   { if (!com(last)) print STOP }
                      { last = $0;
                        sub(/^; ?/,"")
                        print $0
                      }
