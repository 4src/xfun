#!/usr/bin/env python3.12
# vim: set ts=2 sw=2 et :
# ---------------------------------------------------------------------------------------
import re,ast,sys,random
from math import log,floor
from math import pi as PI
from math import e  as E
from fileinput import FileInput as file_or_stdin

class o:
  def __init__(i,**d): i.__dict__.update(d)
  def __repr__(i): 
    rnd = lambda x: round(x,the.round) if isinstance(x,float) else x
    return i.__class__.__name__+"("+", ".join([f"{k}={rnd(v)}" 
                                              for k,v in i.__dict__.items()])+")"

the = o(
  seed  = 1234567891, 
  round = 2,
  train = "data/misc/auto93.csv", 
  bayes = o(k     = 1,
            m     = 2,
            any   = 100,
            best  = 0.5,
            label = 4,
            Last  = 30),
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
# ---------------------------------------------------------------------------------------
class DATA(o):
  def __init__(i)      : i.rows, i.cols = [], o(all=[],x=[],y=[],names=[])
  def add(i,row)       : (i.data if i.cols.all else i.head)(row)
  def clone(i,rows=[]) : return DATA().fromList([i.cols.names]).fromList(rows)
  def chebyshev(i,row) : return max(abs(c.goal - c.norm(row[c.at])) for c in i.cols.y)
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
# ---------------------------------------------------------------------------------------
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
    return x == "?" or i.lo == i.hi and x==i.lo or i.lo < x <= i.hi 

# todo: recode this as functools.reduce using a reduce defined in BIN
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
        b = col.bin(x) ;   
        out[b] = out.get(b,None) or BIN(col.txt,col.at,x)
        out[b].add(x, y(row)) 
  return  mergeBins(col, enough, sorted(out.values(), key=lambda b:b.ymid)) 

# recode this without b4
def mergeBins(col, enough, bins):
  "return two bins that give the most reduction in overall y-diversity"
  if isNum(col): return bins
  most, out = -1 , None
  b4 = bins2bin(bins) 
  for j in range(1,len(bins)):
    one, two = bins2bin(bins[:j]), bins2bin(bins[j:]) 
    here = (one.n * (one.ydiv  - b4.ydiv)**2 + two.n * (two.ydiv  - b4.ydiv)**2)/(one.n + two.n)
    if here > most and one.n > enough and two.n > enough:
      most, out = here, [one, two]
      one.lo, two.hi = -1E32, 1E32
      two.lo = one.hi
  return out
# ---------------------------------------------------------------------------------------
class TREE(o):
  def __init__(i,here,lvl,bin=None,ymid=0):
    i.here, i.lvl, i.bin, i.ymid, i.kids = here, lvl, bin,  ymid, []

  def __repr__(i):
    return f"{i.ymid} {len(i.here.rows)} {'|.. '*(i.lvl-1)} {'' if i.lvl==0 else i.bin}"

  def nodes(i):
    yield i
    print(">>",len(i.kids))
    for kid in i.kids: 
      for sub in kid.nodes():
        yield sub 

def tree(data,rows=None, stop=None):
  def grow(rows, stop=None, lvl=0, above=None):
    stop = stop or len(rows)**0.5
    tree = TREE(data.clone(rows), lvl, above) 
    for bin in bestSplitter(data,rows):
      sub = bin.selects(rows)
      print(len(sub))
      if len(sub) < len(rows) and len(sub) > stop: 
        tree.kids.append(grow(sub, stop=stop, lvl=lvl+1, above=bin))
    return tree
  return grow(rows or data.rows)
  
def bestSplitter(data,rows):
  out, least = [], 1E32, 
  for col in data.cols.x:
    bins = [b for b in makeBins(col, rows, data.chebyshev,len(data.rows)**the.bins.enough)]
    tmp  = bins2bin(bins)  
    if tmp.ydiv < least:
      least = tmp.ydiv
      if tmp.ydiv < least:
        least = tmp.ydiv
        out   = bins
  print("bins",bins)
  return sorted(out, key=lambda b:b.ymid)
# ---------------------------------------------------------------------------------------
def loglikes(data, row, nall, nh): 
  prior = (len(data.rows) + the.bayes.k) / (nall + the.bayes.k*nh)
  likes = [like(col, row[col.at], prior) for col in data.cols.x if row[col.at] != "?"]
  return sum(log(x) for x in likes + [prior] if x>0)

def like(col, x, prior) :
  if not isNum(col):  
    (col.has.get(x,0) + the.bayes.m*prior) / (col.n+the.bayes.m)
  else: 
    v     = col.div()**2 + 1E-30
    nom   = E**(-1*(x - col.mid())**2/(2*v)) + 1E-30
    denom = (2*PI*v) ** 0.5
    return min(1, nom/(denom + 1E-30))
  
def smo(data, score=lambda B,R: B-R): 
  def guess(todo, done):
    cut  = int(.5 + len(done) ** the.bayes.best)
    best = data.clone(done[:cut])
    rest = data.clone(done[cut:])
    key  = lambda r: score(loglikes(best, r, len(done), 2),
                           loglikes(rest, r, len(done), 2))
    random.shuffle(todo) # optimization: only sort a random subset of todo 
    return sorted(todo[:the.bayes.any], key=key, reverse=True) + todo[the.bayes.any:]

  def smo1(todo, done):
    for _ in range(the.bayes.Last - the.bayes.label):
      if len(todo) < 3: break
      top,*todo = guess(todo, done)
      done += [top]
      done = data.clone(done).sort() 
    return done 

  random.shuffle(data.rows) # remove any  bias from older runs
  return smo1(data.rows[the.bayes.label:], data.clone(data.rows[:the.bayes.label]).sort())
# ---------------------------------------------------------------------------------------
def isNum(x): return isinstance(x,NUM)

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
# ---------------------------------------------------------------------------------------
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
    for n,row in enumerate(d.rows) :
       if  n % 30 == 0: print(n,row,round(d.chebyshev(row),2))

  def clone(file):  
    "[FILE]:test loading DATA fromFile" 
    d1 = DATA().fromFile(file or the.train).sort()
    d2 = d1.clone(d1.rows)
    for col1,col2 in zip(d1.cols.all, d2.cols.all):
      print(col1)
      print(col2)

  def norm(file):
    "[FILE]:test normalization"
    d = DATA().fromFile(file or the.train).sort()
    for j,row in enumerate(d.rows):
      for col in d.cols.x:
        row[col.at] = col.bin(row[col.at])

  def bins(file):
    "[FILE]:test bin generation"
    d = DATA().fromFile(file or the.train)
    n=NUM(); [n.add(d.chebyshev(row)) for row in d.rows] 
    enough =  len(d.rows)**the.bins.enough 
    for col in d.cols.x: 
      for bin in [b for b in makeBins(col, d.rows, d.chebyshev, enough)]:
        print(bin,bin.n,bin.ymid)

  def tree(file):
    "[FILE]:test bin generation"
    d = DATA().fromFile(file or the.train)
    for node in tree(d,d.rows,stop=10).nodes(): print(node)

  def smo(file):
    "[FILE]:test bin generation"
    d = DATA().fromFile(file or the.train)
    print(d.chebyshev(smo(d).rows[0]))

def main(a):
  random.seed(the.seed )
  getattr(eg, a[1][1:],"h")(coerce(a[2]) if len(a)>2 else the.train)

if __name__ == "__main__" and len(sys.argv) > 1:  main(sys.argv)
