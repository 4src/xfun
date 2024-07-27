#!/usr/bin/env python3 -B
# vim: set ts=2 sw=2 et :
from fileinput import FileInput as file_or_stdin
import os,re,ast,sys,random
from math import log,floor
any = random.choice
big = 1E30

class o:
  __init__ = lambda i,**d : i.__dict__.update(d)
  __repr__ = lambda i     : i.__class__.__name__+"("+pretty(i.__dict__)+")"

the = o(
  go    = "help",
  round = 2,
  seed  = 1234567891,
  train = "data/misc/auto93.csv",
  dist  = o(far=30,
            xpand=100,
            p=2,
            stop=0.5))

def DATA()            : return o(cols=None, rows=[])
def COLS(names)       : return o(names=names, all=[], x=[], y=[])
def SYM(at=0,txt=" ") : return o(at=at, txt=txt, n=0, nump=False, has={})
def NUM(at=0,txt=" ") : return o(at=at, txt=txt, n=0, nump=True,  lo=big, hi=-big,
                                 goal= 0 if txt[-1] == "-" else 1)

#------------------------------------------------------------------------------
def rows2data(data,src=[]):
  [row2data(data,row) for row in src]
  return data

def row2data(data,row):
  if    data.cols: data.rows += [row2cols(data.cols,row)]
  else: data.cols = cols_init(COLS(row))

def cols_init(cols):
  for at,txt in enumerate(cols.names):
    a,z       = txt[0],txt[-1]
    col       = (NUM if a.isupper() else SYM)(at,txt)
    cols.all += [col]
    if z != "X":
      (cols.y if z in "+-" else cols.x).append(col)
  return cols

def row2cols(cols, row):
  def sym(col,x): col.has[x] = 1 + col.has.get(x,0)
  def num(col,x):
    col.lo = min(x, col.lo)
    col.hi = max(x, col.hi)
  for tmp in [cols.x, cols.y]:
    for col in tmp:
      x = row[col.at]
      if x != "?":
        col.n += 1
        (num if col.nump else sym)(col,x)
  return row        

#------------------------------------------------------------------------------
def norm(col,x):
  if col.nump and x != "?":
    x = (x - col.lo) / (col.hi - col.lo + 1E-32) 
  return x
    
def dists(data,row1,row2):
  d,n = 0,1E-32
  for col in data.cols.x:
    n += 1
    d += dist(col, row1[col.at], row2[col.at]) ** the.dist.p
  return (d/n) ** (1/the.dist.p)

def dist(col,x,y):
  if x==y=="?" : return 1
  if not col.nump: return x != y 
  x,y = norm(col,x), norm(col,y)
  x = (0 if y > 0.5 else 1) if x == "?" else x
  y = (0 if x > 0.5 else 1) if y == "?" else y
  return abs(x - y)

def neighbors(data, row1, rows):
  return sorted(rows or data.rows, key=lambda row2: dists(data,row1,row2))

def twoFar(data, rows, n=None) :
  n = n or the.dist.far
  return max(((any(rows),any(rows)) for _ in range(n)), key= lambda two: dists(data,*two))

def half(data,rows):
  left,right = twoFar(data,rows)
  toLeft = dists(data,left,right)/2
  lefts,rights = [],[]
  for j,row in enumerate(rows):
    (lefts if dists(data,left,row) <= toLeft  else rights).append(row)
  return lefts, rights, left, right, toLeft

def cluster(data,rows,stop=None):
  return _cluster(data, rows, 0, stop, None, None)

def _cluster(data, rows, lvl, stop, when, border):
  def ok(kids): return len(kids) > max(6,stop) and len(kids) < len(rows)
  ls, rs, left, right, toLeft = half(data,rows)
  node = o(rows=rows, lvl=lvl, when=when, border=border, 
           left=left, right=right, lefts=None, rights=None)
  if ok(rows,ls,stop): node.lefts  = _cluster(data, ls, lvl+1, stop, le, toLeft)
  if ok(rows,rs,stop): node.rights = _cluster(data, rs, lvl+1, stop, gt, toLeft)
  return node

def clusters(clustr):
  if clustr:
    print(f"{'|.. '*clustr.lvl}{len(clustr.rows)}")
    clusters(clustr.lefts)
    clusters(clustr.rights)

#----------------------------------------------------
le = lambda x,y: x <= y
gt = lambda x,y: x >  y
r2 = lambda x  : round(x,2)

def pretty(d):
  short = lambda v : round(v,the.round) if isinstance(v,float) else v
  return " ".join(f":{k} {short(v)}" for k,v in d.items())

def coerce(s):
  try: return ast.literal_eval(s)
  except Exception:  return s

def csv(file="-"):
  with file_or_stdin(None if file=="-" else file) as src:
    for line in src:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line: yield [coerce(s.strip()) for s in line.split(",")]

#----------------------------------------------------
def main():
  random.seed(the.seed)
  print(DATA())
  d= rows2data(DATA(), csv(the.train))
  [print(col) for col in d.cols.y]
  for r in d.rows: assert(0 <= dists(d,r,d.rows[0])  <= 1) 
  for r in random.choices(d.rows,k=30): print(len(neighbors(d,r,d.rows)),end=" ")
  for _ in range(10):
    x,y=twoFar(d,d.rows)
    print(r2(dists(d,x,y)),x,y,sep="\t")
  for _ in range(10):
    ls,rs,l,r,c = half(d,d.rows)
    print(len(ls), len(rs), r2(c))
  clusters(cluster(d, d.rows)) #int(log(len(d.rows),2)*the.dist.xpand))))

if __name__ == "__main__": main()
