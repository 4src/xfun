#!/usr/bin/env python3.12 -B
# vim: set ts=2 sw=2 et :
from fileinput import FileInput as file_or_stdin
import re,ast,sys,random
from math import log,floor
any=random.choice

#----------------------------------------------------------------------------------------
class o:
  __init__ = lambda i,**d : i.__dict__.update(d)
  __repr__ = lambda i     : i.__class__.__name__+"("+pretty(i.__dict__)+")"

the = o(
  go    = "help",
  round = 2,
  seed  = 1234567891,
  train = "data/misc/auto93.csv",
  dist  = o(far=30,
            p=2,
            stop=10))

#----------------------------------------------------------------------------------------
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
#----------------------------------------------------------------------------------------
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

  def dist(i, row1, row2):
    n = sum(col.dist(row1[col.at], row2[col.at])**the.dist.p for col in i.cols.x)
    return (n / len(i.cols.x))**(1/the.dist.p)

  def neighbors(i, row1, rows):
    return sorted(rows or o.rows, key=lambda row2: i.dist(row1,row2))

  def twoFar(i, rows, samples=None) :
    return max(((any(rows),any(rows)) for _ in range(samples or the.dist.far)),
                key= lambda two: i.dist(*two))

  def half(i,rows):
    left,right = i.twoFar(rows)
    toLeft = i.dist(left,right)/2
    lefts,rights = [],[]
    for row in rows:
      (lefts if i.dist(row,left) <= toLeft else rights).append(row)
    return lefts, rights, left, right, toLeft

  def cluster(i, rows, lvl=0, guard=None):
    ls, rs, left, right, toLeft = i.half(rows)
    it = TREE(i.clone(rows), lvl, guard,left,right,toLeft)
    if it.ok2go(ls): it.lefts  = i.cluster(ls, lvl+1, lambda r: i.dist(r,left) <= toLeft)
    if it.ok2go(rs): it.rights = i.cluster(rs, lvl+1, lambda r: i.dist(r,left) > toLeft)
    return it

  def predict(i,tree,row):
    leaf     = tree.leaf(row).here
    r1,r2,*_ = i.neighbors(row, leaf.rows)
    print(r1,r2,end=", ")
    d1,d2    = i.dist(r1,row),i.dist(r2,row)
    w1,w2    = 1/(1 if d1 ==0 else d1)**2, 1/(1 if d2==0 else d2)**2
    #return {c.at: (r1[c.at]  + r2[c.at]) / 2 for c in here.cols.y}
    return {c.at: ((r1[c.at] * w1) + (r2[c.at] * w2)) / (w1 + w2) for c in i.cols.y}

class TREE(o):
  def __init__(i, here,lvl,guard,left,right,toLeft):
    i.here, i.lvl,i.guard = here, lvl, guard
    i.left, i.right, i.toLeft = left, right, toLeft
    i.lefts, i.rights = None, None

  def ok2go(i,rows)  : return len(rows) > the.dist.stop and len(rows) < len(i.here.rows)
  def go(i,row)      : return i.guard(row)
  def nogo(i,row)    : return not i.guard(row)

  def __repr__(i):
     return f"{'|.. '*i.lvl}{len(i.here.rows)}"

  def nodes(i):
    yield i
    for kid in [i.lefts,i.rights]:
      if kid:
        for j in kid.nodes(): yield j

  def leaf(i,row) :
    for kid in [i.lefts,i.rights]:
      if kid and kid.guard(row): return kid.leaf(row)
    return i

  #----------------------------------------------------------------------------------------
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

def cli(settings):
  now = settings.__dict__
  for  j,arg in enumerate(sys.argv):
    now = _cli(settings, now, arg[2:]       if len(arg) > 2  else None,
                              arg[1]        if len(arg) == 2 else None,
                              sys.argv[j+1] if j < len(sys.argv) - 1 else "")

def _cli(settings,now,new,flag,val):
  if new in settings.__dict__: 
    now = settings.__dict__[new].__dict__
  else:
    for k,v in now.items():
      if k[0]==flag: now[k]= coerce("False" if v==True else ("True" if v==False else val))
  return now
#----------------------------------------------------------------------------------------
class eg:
  def options(): print(the)

  def help()   : print("./tree.py [ARG] -g action")

  def data():
    d=DATA().fromFile(the.train)
    print(d.cols.y[1])

  def dist():
    d=DATA().fromFile(the.train)
    # print(sorted(round(d.dist(any(d.rows),d.rows[0]),2) for _ in range(30)))
    # random.shuffle(d.rows)
    # d.neighbors(d.rows[0], d.rows[:30])
    # for samples in [10,20,30,40,80,160,320]:
    #   x,y = d.twoFar(d.rows,samples)
    #   print(samples, round(d.dist(x,y),3)) 
    # print("")
    # ls,rs,l,r,c=d.half(d.rows)
    # print(len(ls), len(rs),c)
    # print(l)
    # print(r)
    # print("")
    t =  d.cluster(d.rows)
    # for n in t.nodes(): print(n)
    # print("")
    for row in [any(d.rows) for _ in range(20)]: 
      print(row, d.predict(t,row))

#----------------------------------------------------------------------------------------
cli(the)
random.seed(the.seed)
getattr(eg, the.go)()

