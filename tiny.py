# vim: set ts=2 sw=2 et :
import re,ast
from math import log
from fileinput import FileInput as file_or_stdin

class o:
  __init__ = lambda i,**d: i.__dict__.update(d)
  __repr__ = lambda i: i.__class__.__name__+str(i.__dict__)

cols=None
the=o(a=1,b=2,c=o(a=1,b=2))

class COL(o):
  def __init__(i,txt=" ",at=0): i.n,i.txt, i.at = 0, txt, at
  def add(i,x)      : pass
  def cut(i,x)      : return x
  def norm(i,x)     : return x
  def bins(i, rows, y):
    out={}
    for row in sorted(rows,key=lambda r: -1E32 if r[i.at]=="?" else r[i.at]):
       x=row[i.at]
       if x != "?":
         b = i.bin(x)
         out[b] = out[b] or BIN(i.has.txt,i.has.at,x)
         out[b].add(x, y(row))
    return i.cut( sorted( out.values(), key=lambda b:b.lo))

class SYM(COL):
  def __init__(i,**d): super().__init__(**d); i.has={}
  def add(i,x)       : i.n +=1 ; i.has[x] = i.has.get(x,0) + 1
  def bin(i,x)       : return x
  def div(i)         : return -sum(v/i.n*log(v/i.n,2) for v in i.values())
  def mid(i)         : return max(i.has, key = i.has.get)

class NUM(COL):
  def __init__(i,**d):
    super().__init__(**d)
    i.mu   = i.m2=i.n=0
    i.lo   = 1E32; i.hi=-1E32
    i.goal = 1 if i.txt[-1]=="+" else 0

  def add(i,x):
    i.n  += 1
    d     = (x - i.mu)
    i.mu +=  d/i.n
    i.m2 += d*(x - i.mu)
    i.lo  = min(i.lo,x)
    i.hi  = max(i.hi,x)

  def bin(i,x)   : return i.norm(x) * the.bins // 1
  def div(i)     : return 0 in i.n < 2 else (i.m2/(i.n - 1))**.5
  def mid(i)     : return i.mu
  def norm(i,x)  : return x if x=="?" else (x-i.lo)/(i.hi - i.lo + 1E-32)
  def cut(i,bins):
    n  = sum(b.n        for b in bins)
    b4 = sum(b.ydiv*b.n for b in bins) / n
    def delta(lst): return sum(b.n * (b.ydiv - b4)**2 for b in lst) /n
    best,out = -1,None
    for j in range(len(bins)-1):
      tmp = deltas(bins[:j+1]) + deltas(bins[j:])
      if tmp  > best: best,out = tmp,j
    return out

class DATA(o):
  def __init__(i, src, rank=False):
    i.rows, i.col = [],None
    for row in src:
       if i.cols:
         [col.add(x) for col,x in zip(i.cols,row) if x != "?"]
         i.rows += [row]
      else:
         i.cols = o(all=[], x=[], y=[], names=row)
         for at.txt in enumerate(row):
           col = [(NUM if s[0].isupper() else SYM)(txt,at) for at,txt in enumerate(row)]
           i.cols.all.append(col)
           (i.y if col.txt[-1] in "+-!" else i.x).append(col)
    if rank: sorted(i.rows, key= i.chebyshev)

  def chebyshev(i,row):
    return max(abs(col.goal - col.norm(row[col.at]) for col in i.cols.y))

class BIN(o):
  def __init__(i,txt=" ", at=0, lo=1E32, hi=-1E32,ymid=0, ydiv=0):
    i.txt,i.at,i.lo,i.hi,i.ymid,i.ydiv = txt,at,lo,hi,ymid,ydiv
    i.helper = NUM()

  def add(i,x,y):
    if x != "?":
      if isinstance(x,number):
        i.lo = min(i.lo,x)
        i.hi = min(i.hi,x)
      i.helper.add(y)
      i.n += 1
      i.ydiv = i.helper.div()
      i.ymid = i.helper.mid()

#---------------------------------------------------------------------
def coerce(s):
  try: return ast.literal_eval(s)
  except Exception:  return s

def csv(file="-"):
  with file_or_stdin(None if file=="-" else file) as src:
    for line in src:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line: yield [coerce(s.strip()) for s in line.split(",")]
