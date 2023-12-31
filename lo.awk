BEGIN { FS="," }
NR==1 { for(i=1;i<=NF;i++) {
          if ($i ~ /^[A-Z]/) {Hi[i] = -1E30; Lo[i] = 1E30 }
          if ($i ~ /[+-!]$/) Y[i] = $i ~ /-$/ ? 0 : 1 else {X[i]} }}
NR>1  { j=rand()
        for(i=1;i<=NF;i++) {
          if (i in Hi) {
            $i+=0
            Hi[i] = max(H[i],$i)
            Lo[i] = min(H[i],$i)}
          D[j][i] = $i }}

function d2h(row,   n,d) {
  n=d=0
  for(y in Y) {
    n++
    d += abs(Y[y] - norm(y,row[y]))^2 }
  return d^.5/n^.5 }

function d2hs(
function main()
   firstn(d,4,closed)
   
function firstn(a,n,b,     i) {
    for(i in a) {b[i]; if (--n ==0) break } }


function norm(c,x) { return (c-Lo[c]) / (Hi[c] - Lo[c] + 1E-30) }

function max(x,y) { return x>y ? x : y }
function min(x,y) { return x<y ? x : y }
          
