{a[NR]=$0}
END {
 for(i in a) 
   if (a[i] != "SKIP") {
     print(a[i])
     if (a[i+2] ~ /^[ \t]*"/)  {
       sub(/^[ \t]+"/,"",a[i+2])
       sub(/"/,"",a[i+2])
       print("; " a[i+2]) 
       a[i+2] = "SKIP"}}}
