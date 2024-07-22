# vim: set ft=awk : set sw=2 ts=2 xt :

function has(a,i)  { i= i=="" ? 1+length(a) : i; a[i][0]; delete a[i][0]; return i }
function isa(i,is) { i["is"] = is }

function Col(i,txt,pos) { i.txt = txt; i.pos = i.n = 0 }

function Num(i,txt,pos) {
  isa(i,"Num")
  Col(txt,pos)
  i.mu = i.m2 = i.sd = 0
  i.goal = txt ~ /-$/ ? -1 : 1  }

function Sym(i,txt,pos) {
  isa(i,"Sym")
  Col(txt,pos)
  has(i,"seen")
  i.mode=""; i.most=0}

function Data(i) { isa(i,"Data"); has(i,"cols"); has(i,"rows") }

function addData(data,row) { data.cols.all ?  addRow(data,row) : addHead(data,row) }

function csv(file,rows,cols,names,     row,n) {
  file = file ? file : "-"
  while( getline file  > 0 ) {
    n++
    s2a($0,a)
    if (n==1) {
      s2a($0,a); head(names,cols) 
    } else {:
      split($0,
for(i in names) names[i] = trim(names[i]);

    addRow(rows,cols,row) }
  close(file) }

function addRow(row,rows,cols) {
  if (cols) { rows[
     
function add(i,x,    f) { f="add" i.is; return @f(i,x) }

#-------------------------------------------------------
function push(a,x) { a[1 + length(a)] = x; return x }

function trim(s) {
  sub(/^[ \t]*/, "", s) 
  sub(/[ \t]*$/, "", s) 
  return s }

function s2a(s,a,     i,x,y) {
  split(s,a,/,/)
  for(i in a) { x=trim(a[i]); y=x+0; a[i]= x=y ? y : x } }
 
BEGIN {a(i); print(i.n) }
