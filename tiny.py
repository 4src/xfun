import re,ast
from math import log
from fileinput import FileInput as file_or_stdin

class o: 
  __init__ = lambda i,**d: i.__dict__.update(d)
  __repr__ = lambda i: i.__class__.__name__+str(i.__dict__)

cols=None
the=o(a=1,b=2,c=o(a=1,b=2))

def nump(col): return isinstance(col,list)

class COL(o):
  def __init__(i,txt=" ",pos=0, has=None):
    i.txt, i.pos = txt,pos

  def add(i,x):
    if nump(i.has): col.append(x)
    else: col[x] = col.get(x,0) + 1 

  def mid(i):
    a=i.has; return a[len(a)//2] if nump(a) else max(a,key=a.get)

  def div(i):
    if nump(col): 
       ten = len(col)//10; return (col[9*ten] - col[ten])/2.56
     n = sum(col.values())
     return - sum(x/n*log(x/n,2) for x in col.values())

  def sorted(i,rows):
    return sorted(rows, key=lambda r: -1E32 if r[i.pos] =="?" else r[i.pos])

  def bin(i,x)
     return i.norm(x) * the.bins // 1 if nump(i.has) else x

  def bins(i, rows, y)
    out={}
    for row in i.sorted(rows):
       x=row[i.pos]
       if x != "?":
         b= i.bin(x)
         out[b] = out[b] or BIN(col.txt,col.pos,x)
         out[b]:add(x, y(row))
    return i.cut(sorted(out.values(),key=lambda b:b.lo)) if nump(col) else out.values() 
  
class NUM(COL): 
   def __init__(i,**d):
     super().__init__(**d) 
     i.has=[]
     i.goal = 1 if i.txt[-1]="+" else 0

  def norm(i,x):
    if x != "?" and nump(i.has):
      lo,hi = i.has[0], i.has[-1]
      return (x-lo)/(hi-lo+1E-32)
    return x

class SYM(COL:
   def __init__(i,**d):
     super().__init(**d) 
     i.has={}

class DATA(o)
  def __init__(i, src, rank=False):
    i.rows, i.col = [],None
    for row in src:
      if i.cols: 
         [add(col,x) for col,x in zip(cols,row) if x != "?"]
         i.rows += [row]
      else: 
         i.cols = [(NUM if s[0].upper() else SYM)(s,pos) for pos,s in enumerate(row)]
         i.x,i.y = [],[]
         for col in i.cols: (y if col.txt[-1] in "+-!" else x).append(col)

    [i.col.sort() for col in cols if nump(col)]
    if rank then sorted(i.rows, key= i.chebyshev) 

  def chebyshev(i,row):
    return max(abs(col.goal - col.norm(row[col.pos)) for col in i.y)
     
def xpect(*bins):
  n,w=0,0
  for bin in bins: 
    n = n + bin.n; w = w + bin.n*bin.ydiv
  return ydiv/n

  
 def cut(bins):
   for i,bin in enumerate(bins):
      tmp=xpect(bins[:i], bins[i:]) 
---------------------------------------------------------------------
def coerce(s:str) -> atom:
  try: return ast.literal_eval(s)
  except Exception:  return s

def csv(file="-") -> row:
  with file_or_stdin(None if file=="-" else file) as src:
    for line in src:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line: yield [coerce(s.strip()) for s in line.split(",")]
