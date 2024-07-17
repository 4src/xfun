#!/usr/bin/env python3.12
# vim: set ts=2 sw=2 et :
import re,ast,sys,random
from math import log,floor
from fileinput import FileInput as file_or_stdin

class o:
  __init__ = lambda i,**d: i.__dict__.update(d)
  __repr__ = lambda i: i.__class__.__name__+str(i.__dict__)

the=o(
  seed  = 1234567891, 
  train = "data/misc/auto93.csv", 
  bins  = o(max=17,
            enough=0.5)
)

class COL(o):
  def __init__(i,txt=" ",at=0): i.n,i.txt, i.at = 0, txt, at
  def add(i,x)  : pass 
  def norm(i,x) : return x

class SYM(COL):
  def __init__(i,**d): super().__init__(**d); i.has={}
  def add(i,x)  : i.n +=1 ; i.has[x] = i.has.get(x,0) + 1
  def bin(i,x)  : return x
  def div(i)    : return -sum(v/i.n*log(v/i.n,2) for v in i.has.values())
  def mid(i)    : return max(i.has, key = i.has.get)

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

  def bin(i,x)  : return floor( i.norm(x) * the.bins.max )
  def div(i)    : return 0 if i.n < 2 else (i.m2/(i.n - 1))**.5
  def mid(i)    : return i.mu
  def norm(i,x) : return x if x=="?" else (x - i.lo)/(i.hi - i.lo + 1E-32)

class DATA(o):
  def __init__(i)      : i.rows, i.cols = [], None
  def add(i,row)       : (i.data if i.cols else i.head)(row)
  def clone(i,rows=[]) : return DATA().fromList([i.cols.names] + rows)
  def chebyshev(i,row) : return max(abs(c.goal - c.norm(row[c.at])) for c in i.cols.y)
  def fromFile(i,file) : [i.add(row) for row in csv(file)]; return i
  def fromList(i,lst)  : [i.add(row) for row in lst      ]; return i 
  def sort(i)          : i.rows.sort(key = i.chebyshev)   ; return i

  def data(i,row): 
    [col.add(x) for col,x in zip(i.cols.all,row) if x != "?"]
    i.rows += [row]

  def head(i,row): 
    i.cols = o(all=[], x=[], y=[], names=row)
    for at,txt in enumerate(row):
      col = (NUM if txt[0].isupper() else SYM)(txt=txt,at=at) 
      i.cols.all.append(col)
      if txt[-1] != "X":
        (i.cols.y if col.txt[-1] in "+-!" else i.cols.x).append(col)

class BIN(o):
  def __init__(i,txt=" ", at=0, n=0, lo=1E32, hi=-1E32,ymid=0, ydiv=0):
    i.txt,i.at,i.lo,i.hi,i.ymid,i.ydiv = txt,at,lo,hi,ymid,ydiv
    i.n, i.yhelper = n, NUM()

  def __repr__(i):
    s,lo,hi = i.txt, i.lo, i.hi
    if lo <= -1E32: return f"{s} <= {hi:.2f}"
    if hi >=  1E32: return f"{s} > {lo:.2f}"
    if lo==hi     : return f"{s} = {lo:.2f}"
    return f"{lo:.2f} < {s} <= {hi:.2f}"

  def add(i,x,y):
    if x != "?":
      if isinstance(x,(int,float)):
        i.lo = min(i.lo,x)
        i.hi = max(i.hi,x)
      i.yhelper.add(y)
      i.n    = i.yhelper.n
      i.ydiv = i.yhelper.div()
      i.ymid = i.yhelper.mid()

  def selects(i,rows): return [row for row in rows if i.select(row)]

  def select(i,row):
    x=row[i.at]
    if x == "?": return True
    return i.lo == i.hi and x==i.lo or i.lo < x <= i.hi 

  @staticmethod
  def generateBins(col, rows, y, enough):
    out={}
    n=1
    for row in sorted(rows,key=lambda r: -1E32 if r[col.at]=="?" else r[col.at]):
       x=row[col.at]
       if x != "?":
         b = col.bin(x) ;   n=n+1
         out[b] = out.get(b,None) or BIN(col.txt,col.at,x)
         out[b].add(x, y(row))
    return BIN.twoBins(col, enough, sorted(out.values(), key=lambda b:b.lo))

  @staticmethod
  def combineBins(bins):
    "Combine N bins into one"
    n, ymids, ydivs, lo, hi = 0, 0, 0, bins[0].lo, bins[0].hi
    for b in bins:
      n      = int(n + b.n)
      lo,hi  = min(lo, b.lo), max(hi, b.hi)
      ymids += b.ymid * b.n
      ydivs += b.ydiv * b.n
    return BIN(bins[0].txt, bins[0].at, lo=lo, hi=hi, n=n, ymid=ymids/n, ydiv=ydivs/n)

  @staticmethod
  def twoBins(col, enough, bins):
    "return two bins that give the most reduction in overall y-diversity"
    if isinstance(col,SYM): return bins
    #bins = merge(bin, enough )
    most, out, b4 = -1, None, BIN.combineBins(bins)
    for j in range(2,len(bins)):
      one, two = BIN.combineBins(bins[:j]), BIN.combineBins(bins[j:])
      diff = one.n/b4.n * one.ydiv + two.n/b4.n * two.ydiv 
      if diff > most and one.n > enough and two.n > enough:
        most, out = diff, [one, two]
        one.lo, two.hi = -1E32, 1E32
        two.lo = one.hi
    return out

def coerce(s):
  try: return ast.literal_eval(s)
  except Exception:  return s

def csv(file="-"):
  with file_or_stdin(None if file=="-" else file) as src:
    for line in src:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line: yield [coerce(s.strip()) for s in line.split(",")]

def printm(matrix):
  s = [[str(e) for e in row] for row in matrix]
  lens = [max(map(len, col)) for col in zip(*s)]
  fmt = ' | '.join('{{:>{}}}'.format(x) for x in lens)
  for row in [fmt.format(*row) for row in s]:  print(row)

#-----------------------------------------------------------
class eg:
  def h(): print("tiny.py -[h] [ARG]")

  def num(_):
    n=NUM()
    [n.add(i**.5) for i in range(100)]
    assert( 6.6 < n.mid() < 6.7)
    assert( 2.4 < n.div() < 2.41)

  def sym(_):
    s=SYM()
    [s.add(c) for c in "aaaabbc"]
    assert("a"==s.mid())
    assert(1.37 < s.div() < 1.38)

  def csv(file):
     n=0
     for row in csv(file or the.train):
      if  n % 30 == 0: print(n,row)
      n += 1

  def train(file):
    d = DATA().fromFile(file or the.train).sort()
    [print(col) for col in d.cols.all]
    for n,row in enumerate(d.rows) :
      if  n % 30 == 0: print(n,row,d.chebyshev(row))

  def norm(file):
    d = DATA().fromFile(file or the.train).sort()
    for j,row in enumerate(d.rows):
      for col in d.cols.x:
        row[col.at] = col.bin(row[col.at])

  def bins(file):
    d = DATA().fromFile(file or the.train)
    print(sum(d.chebyshev(row) for row in d.rows)/len(d.rows))
    enough = len(d.rows)**the.bins.enough 
    out,least = None,1E32
    for col in d.cols.x:
      bins = sorted([b for b in BIN.generateBins(col, d.rows, d.chebyshev, enough)], key=lambda b:b.ymid)
      tmp= BIN.combineBins(bins) 
      print(col.txt, tmp.ydiv) 
      if tmp.ydiv < least:
        least=tmp.ydiv
        out = bins
    print([b.ymid for b in out])

def main(a):
  random.seed(the.seed )
  getattr(eg, a[1][1:],"h")(coerce(a[2]) if len(a)>2 else the.train)

if __name__ == "__main__" and len(sys.argv) > 1:  main(sys.argv)
