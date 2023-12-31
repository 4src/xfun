BEGIN { FS="\n"; RS=""
        print "# " file "\n" }
      { a[NR] = $0     }
END   { for(i=1; i<=NR; i++) main(i, a[i]) }

function src(i)  { return a[i] ~ /^[(]/ }

function main(i,s,srcp,    pre,post) {
  if (src(i)) s = "\n```lisp\n"  s "\n```\n"
  sub(/^; /,  "",  s)
  gsub(/\n; /,"\n",s)
  print s  }
