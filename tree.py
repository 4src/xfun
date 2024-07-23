#!/usr/bin/env python3.12
# vim: set ts=2 sw=2 et :
from fileinput import FileInput as file_or_stdin
import re,ast,sys,random
from math import log,floor
any=random.choice

class o:
  __init__ = lambda i,**d : i.__dict__.update(d)
  __repr__ = lambda i     : i.__class__.__name__+"("+str(i.__dict__)+")"

the = o(train="data/misc/config")

the = o(
  seed  = 1234567891,
  round = 2,
  train = "data/misc/auto93.csv",
  dist  = o(p=2,
            far=30,
            stop=10))

class COL(o):
  def __init__(i,txt=" ",at=0): i.n,i.txt, i.at = 0, txt, at
  def add(i,x)  : pass
  def norm(i,x) : return x

class SYM(COL):
  def __init__(i,**d): super().__init__(**d); i.has={}
  def add(i,x)    : i.n +=1 ; i.has[x] = i.has.get(x,0) + 1
  def div(i)      : return -sum(v/i.n*log(v/i.n,2) for v in i.has.values())
  def mid(i)      : return max(i.has, key = i.has.get)
  def dist(i,x,y) : return  x==y=="?" and 1 or x != y

class NUM(COL):
  def __init__(i,**d):
    super().__init__(**d)
    i.mu   = i.m2 = i.n = 0
    i.lo   = 1E32; i.hi=-1E32
    i.goal = 1 if i.txt[-1]=="+" else 0

  def add(i,x):
    i.n  += 1
    d     = (x - i.mu)
    i.mu +=  d/i.n
    i.m2 += d*(x - i.mu)
    i.lo  = min(i.lo, x)
    i.hi  = max(i.hi, x)

  def div(i)    : return 0 if i.n < 2 else (i.m2/(i.n - 1))**.5
  def mid(i)    : return i.mu
  def norm(i,x) : return x if x=="?" else (x - i.lo)/(i.hi - i.lo + 1E-32)

  def dist(i,x,y):
    if x==y=="?": return 1
    x, y = i.norm(x), i.norm(y)
    x = x if x !="?" else (1 if y<0.5 else 0)
    y = y if y !="?" else (1 if x<0.5 else 0)
    return abs(x-y)

class DATA(o):
  def __init__(i)      : i.rows=[];  i.cols = o(all=[],x=[],y=[],names=[])
  def add(i,row)       : (i.data if i.cols.all else i.head)(row)
  def clone(i,rows=[]) : return DATA().fromList([i.cols.names] + rows)
  def fromFile(i,file) : [i.add(row) for row in csv(file)]; return i
  def fromList(i,lst)  : [i.add(row) for row in lst      ]; return i
  def sort(i)          : i.rows.sort(key = i.chebyshev)   ; return i

  def data(i,row):
    [col.add(x) for col,x in zip(i.cols.all,row) if x != "?"]
    i.rows += [row]

  def head(i,row):
    i.cols.names = row
    for at,txt in enumerate(row):
      col = (NUM if txt[0].isupper() else SYM)(txt=txt,at=at)
      i.cols.all.append(col)
      if txt[-1] != "X":
        (i.cols.y if col.txt[-1] in "+-!" else i.cols.x).append(col)

  def dists(i, row1, row2):
    n = sum(col.dict(row1[col.at], row2[col.at])**the.dist.p for col in i.cols.x)
    return (n / len(i.cols.x))**(1/the.dist.p)

  def neighbors(i, row1, rows):
    return sorted(rows or o.rows, key=lambda row2: i.dists(row1,row2))

  def twoFar(i, rows) :
    return max(((any(rows),any(rows)) for _ in range(the.dists.far)),
                key= lambda two: i.dists(*two))

  def half(i,rows):
    left,right = i.twoFar(rows)
    toLeft = i.dist(left,right)/2
    lefts,rights = [],[]
    for i,row in rows:
      (lefts if i.dists(row,left) <= toLeft else rights).append(row)
    return lefts, rights, left, right, toLeft

  def cluster(i, rows, lvl=0, guard=None):
    it = TREE(i.clone(rows), lvl, guard)
    ls, rs, it.left, it.right, it.toLeft = i.half(rows)
    go = lambda row: i.dists(row,it.left) <= it.toleft 
    if it.ok2go(ls): it.lefts  = i.cluster(ls, lvl+1, guard = go)
    if it.ok2go(rs): it.rights = i.cluster(rs, lvl+1, guard = lambda row: not go(row))
    return it

class TREE(o):
  def __init__(i, here,lvl,guard): i.here, i.lvl, i.guard = here, lvl, guard
  def ok2go(i,rows)  : return len(rows) > the.dist.stop and len(rows) < len(i.here.rows)
  def go(i,row)      : return i.guard(row)
  def nogo(i,row)    : return not i.guard(row)

  def leaf(i,row) :
    for kid in [i.lefts,i.rights]:
      if kid.guard(row): return kid.leaf(row)
    return i

  def predict(i,row):
    here     = i.leaf(row).here
    r1,r2,*_ = here.neighbors(row)
    d1,d2    = here.dists(r1,row), here.dists(r2,row)
    w1,w2    = 1/d1**2, 1/d2**2 
    return {c.at: ((r1[c.at] * w1) + (r2[c.at] * w2) / (w1 + w2)) for c in here.cols.y} 

def coerce(s):
  try: return ast.literal_eval(s)
  except Exception:  return s

def csv(file="-"):
  with file_or_stdin(None if file=="-" else file) as src:
    for line in src:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line: yield [coerce(s.strip()) for s in line.split(",")]