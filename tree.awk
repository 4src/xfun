BEGIN {FS=","
       FARS=20
       P=2}

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

function dists(row1,row2,     n,d) {
  for (c in row1)
    if (!(c in Y)) {
      n++
      d += dist(c, row1[c], row2[c])**P }
  return (d/n)**(1/P)

function dist(c, x,y) {
  if (x=="?" && y=="?" ) return 1
  x = norm(c,x)
  y = norm(c,y)
  x = x=="?" ? (y > 0.5 ? 0 : 1) : x
  y = y=="?" ? (x > 0.5 ? 0 : 1) : y
  return abs(x-y) }

function norm(c,x) {
  if ((c in Lo) && (x != "?")) {
    x = (x - Lo(c))/(Hi[c] - Lo[c] + 1E-32) }
  return x }

function twoFar(rows) {
  for(i=1;i<=FARS;i++) {
    j=any(rows)
    k=any(rows)
    d=dists(rows[j], rows[k])
    if (
    :sp

function any(a) { return int(0.5 + rand()*length(a)) }
function abs(x) { return x<0 ? -x : x }

