#!/usr/bin/env python3.12
# vim: set ts=2 sw=2 et :
import re,ast,sys,random
from math import log,floor
from fileinput import FileInput as file_or_stdin

class o:
  __init__ = lambda i,**d: i.__dict__.update(d)
  __repr__ = lambda i: i.__class__.__name__+str(i.__dict__)

the = o(
  seed  = 1234567891, 
  train = "data/misc/auto93.csv", 
  bins  = o(max    = 17,
            enough = 0.5))

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
# ----------------------------------------------------------------------------------------
class DATA(o):
  def __init__(i)      : i.rows, i.cols = [], o(all=[],x=[],y=[],names=[])
  def add(i,row)       : (i.data if i.cols else i.head)(row)
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
# ----------------------------------------------------------------------------------------
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

def bins2bin(bins):
  "Combine N bins into one"
  n, ymids, ydivs, lo, hi = 0, 0, 0, bins[0].lo, bins[0].hi
  for b in bins:
    n      = int(n + b.n)
    lo,hi  = min(lo, b.lo), max(hi, b.hi)
    ymids += b.ymid * b.n
    ydivs += b.ydiv * b.n
  return BIN(bins[0].txt, bins[0].at, lo=lo, hi=hi, n=n, ymid=ymids/n, ydiv=ydivs/n)

def makeBins(col, rows, y, enough):
  out={}
  for row in sorted(rows,key=lambda r: -1E32 if r[col.at]=="?" else r[col.at]):
      x=row[col.at]
      if x != "?":
        b = col.bin(x) ;   n=n+1
        out[b] = out.get(b,None) or BIN(col.txt,col.at,x)
        out[b].add(x, y(row))
  return mergeBins(col, enough, sorted(out.values(), key=lambda b:b.lo))

def mergeBins(col, enough, bins):
  "return two bins that give the most reduction in overall y-diversity"
  if isinstance(col,SYM): return bins
  most, out = -1, None
  for j in range(2,len(bins)):
    one, two = bins2bin(bins[:j]), bins2bin(bins[j:])
    here = one.n * one.ydiv + two.n * two.ydiv 
    if here > most and one.n > enough and two.n > enough:
      most, out = here, [o, two]
      one.lo, two.hi = -1E32, 1E32
      two.lo = one.hi
  return out
# ----------------------------------------------------------------------------------------
class TREE(o):
  def __init__(i,here,lvl,bin):
    i.here, i.lvl, i.bin, i.kids = here, lvl, bin, []

  def __repr__(i):
    return f"{i.mu} {len(i.here.rows)} {'|.. '*i.lvl-1} {'' if i.lvl==0 else i.bin}"

  def nodes(i):
    yield i
    for kid in i.kids: 
      for sub in kid.nodes():
        yield sub 

def tree(data,rows=None, stop=None):
  def grow(rows, stop=None, lvl=0, above=None):
    stop = stop or len(rows)**0.5
    tree = TREE(data.clone(rows), lvl, above) 
    for bin in bestSplitter(data,rows):
      sub = bin.selects(rows)
      if len(sub) < len(rows) and len(sub) > stop: 
        tree.kids.append(grow(sub, stop=stop, lvl=lvl+1, above=bin))
    return tree
  return grow(rows or data.rows)
  
def bestSplitter(data,rows):
  out, least = [], 1E32, 
  for col in data.cols.x:
    bins= [b for b in makeBins(col, rows, data.chebyshev,
                                          len(data.rows)**the.bins.enough)]
    tmp  = bins2bin(bins)  
    if tmp.ydiv < least:
      least = tmp.ydiv
      if tmp.ydiv < least:
        least = tmp.ydiv
        out   = bins
  return sorted(out, key=lambda b:b.ymid)
# ----------------------------------------------------------------------------------------
def coerce(s):
  try: return ast.literal_eval(s)
  except Exception:  return s

def csv(file="-"):
  with file_or_stdin(None if file=="-" else file) as src:
    for line in src:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line: yield [coerce(s.strip()) for s in line.split(",")]

def prints(matrix):
  s = [[str(e) for e in row] for row in matrix]
  lens = [max(map(len, col)) for col in zip(*s)]
  fmt = ' | '.join('{{:>{}}}'.format(x) for x in lens)
  for row in [fmt.format(*row) for row in s]:  print(row)
# ----------------------------------------------------------------------------------------
class eg:
  def egs(_):
    ":show all examples"
    for s in dir(eg): 
      if s[0] != "_":
        a = getattr(eg,s).__doc__.split(":")
        print(f"  -{s:5} {a[0]}\t  {a[1]} ")

  def h(_): 
    ":show help"
    print("ezr.py -[h|seed|egs|OTHERS] [ARG]")
    print("ezr.py -egs (to list all the OTHER actions)")

  def seed(n): 
    ":set seed"
    random.seed(n); print(random.random())
    the.seed = n

  def num(_):
    ":test NUM class"
    n=NUM()
    [n.add(i**.5) for i in range(100)]
    assert( 6.6 < n.mid() < 6.7)
    assert( 2.4 < n.div() < 2.41)

  def sym(_):
    ":test SYM class"
    s=SYM()
    [s.add(c) for c in "aaaabbc"]
    assert("a"==s.mid())
    assert(1.37 < s.div() < 1.38)

  def csv(file):
    "[FILE]:test csv file reading"
    n=0
    for row in csv(file or the.train):
      if  n % 30 == 0: print(n,row)
      n += 1

  def train(file):
    "[FILE]:test loading DATA fromFile"
    d = DATA().fromFile(file or the.train).sort()
    [print(col) for col in d.cols.all]
    for n,row in enumerate(d.rows) :
      if  n % 30 == 0: print(n,row,d.chebyshev(row))

  def norm(file):
    "[FILE]:test normalization"
    d = DATA().fromFile(file or the.train).sort()
    for j,row in enumerate(d.rows):
      for col in d.cols.x:
        row[col.at] = col.bin(row[col.at])

  def bins(file):
    "[FILE]:test bin generation"
    d = DATA().fromFile(file or the.train)
    out,least, enough = None,1E32, len(d.rows)**the.bins.enough 
    for col in d.cols.x:
      bins= sorted([b for b in makeBins(col, d.rows, d.chebyshev, enough)], 
                   key=lambda b:b.ymid)
      tmp= bins2bin(bins)  
      if tmp.ydiv < least:
        least=tmp.ydiv
        out = bins
    print([b.ymid for b in out])

def main(a):
  random.seed(the.seed )
  getattr(eg, a[1][1:],"h")(coerce(a[2]) if len(a)>2 else the.train)

# ----------------------------------------------------------------------------------------
if __name__ == "__main__" and len(sys.argv) > 1:  main(sys.argv)
