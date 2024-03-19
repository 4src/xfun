BEGIN  { FS=","; OFS=" "; print "("}
       { gsub(/[ \t\r]/,"") }
NR==1  { printf("(")
         for(i=1;i<=NF;i++) printf("\"%s\" ",$i)
         print ")"}
NR>1   { printf("(")
         for(i=1;i<=NF;i++) printf("%s ",$i)
         print ")"}


END    { print ")" }
