BEGIN {FS=","}
/^----/     {n++}
/^#/       { next }
/^[ \t]*$/ { next }
           { gsub(/[ \t]*/,"") }
NF> 6      { a[$2][$1]++ }
END    { for(x in a) {
           print(x)
           for(y in a[x])
              print("\t",y,int(100*a[x][y]/n))
}}