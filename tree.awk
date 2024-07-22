BEGIN {FS=","}
      {gsub(/[ \t]+/,"")
      for(i=1;i<=NF;i++) if ($i != "?") { x= $i; y= $i+0; $i= x==y ? y : x}}
NR==1 {for(i=1;i<=NF;i++) { 
         Names[i]=$i;  
         if ($i ~ /X$/) continue
         if ($i ~ /^[A-Z]/) {Lo[i]= 1E32; Hi[i]= -1E32}
         if ($i ~ /[!+-]/ ) Y[i] }}
{ split($0, rows[NR-1],/,/)
  for(i in Lo) if ($i != "?" && $i < Lo[i]) Lo[i] = $i
  for(i in Hi) if ($i != "?" && $i > Hi[i]) Hi[i] = $i
}
END { for(i in Hi) print Lo[i],Hi[i]} 