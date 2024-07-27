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

def norm(col,x):
  if col.nump and x != "?":
    x = (x - col.lo) / (col.hi - col.lo + 1E-32) 
  return x
#------------------------------------------------------------------------------
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
  k    = log(len(rows),2) * the.dist.xpand
  print(k)
  rows = rows if k >= len(rows) else random.choices(rows, k=int(k))
  stop = stop or len(rows) ** the.dist.stop
  return _cluster(data, rows, 0, stop, None, None)

def _cluster(data, rows, lvl, stop, when, border):
  def ok(kids): 
    return len(kids) > max(6,stop) and len(kids) < len(rows)

  ls, rs, left, right, toLeft = half(data,rows)
  node = o(rows=rows, lvl=lvl, when=when, border=border, 
           left=left, right=right, lefts=None, rights=None)
  if ok(ls): node.lefts  = _cluster(data, ls, lvl+1, stop, le, toLeft)
  if ok(rs): node.rights = _cluster(data, rs, lvl+1, stop, gt, toLeft)
  return node

def clusters(tree):
  if tree:
    print(f"{'|.. '*tree.lvl}{len(tree.rows)}")
    clusters(tree.lefts)
    clusters(tree.rights)

def leaf(data, clustr, row):
  for kid in [clustr.lefts, clustr.rights]:
    if kid and kid.when:
      if kid.when( dists(data, kid.left, row), kid.border):
         return leaf(data, kid, row)
  return clustr

def knn2(data,tree,row):
  rows     = leaf(data,tree,row).rows
  r1,r2,*_ = neighbors(data,row, rows)
  d1,d2    = dists(data,r1,row),dists(data, r2,row)
  if d1==0:  return {c.at: r1[c.at] for c in data.cols.y}
  if d2==0:  return {c.at: r2[c.at] for c in data.cols.y}
  w1,w2    = 1/d1**2, 1/d2**2
  return {c.at: ((r1[c.at] * w1) + (r2[c.at] * w2)) / (w1 + w2) for c in data.cols.y}

#----------------------------------------------------
le = lambda x,y: x <= y
gt = lambda x,y: x >  y
r2 = lambda x  : round(x,2)

def musd(a, fun=lambda x:x):
  a = sorted(fun(x) for x in a if x != "?")
  n = len(a)//10
  return a[5*n], (a[9*n] - a[n]) / 2.56

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
  the.train = os.environ.get("TRAIN",the.train)
  the.seed  = os.environ.get("SEED", the.seed)
  random.seed(the.seed)
  d= rows2data(DATA(), csv(the.train))
  print("\nycols ...")
  [print(col) for col in d.cols.y]
  print("\ndistances ok ...")
  for r in d.rows: assert(0 <= dists(d,r,d.rows[0])  <= 1) 
  print("\ntwoFar ...")
  for _ in range(10):
    x,y = twoFar(d,d.rows)
    print(r2(dists(d,x,y)),x,y,sep="\t")
  print("\ndividing ...")
  for _ in range(10):
    ls,rs,l,r,c = half(d,d.rows)
    print(r2(c), len(ls), len(rs),sep="\t")
  print("\nclusters ...")
  tree= cluster(d, d.rows) #int(log(len(d.rows),2)*the.dist.xpand))))
  clusters(tree) #int(log(len(d.rows),2)*the.dist.xpand))))
  print(knn2(d,tree,d.rows[0]))
  print(musd(d.rows, lambda row:row[-1]))

if __name__ == "__main__": main()
