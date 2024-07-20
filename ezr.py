#!/usr/bin/env python3.12
# vim: set ts=2 sw=2 et :
import re,ast,sys,random
from math import log,floor
from math import e as E
from math import pi as PI
from fileinput import FileInput as file_or_stdin

class o:
  __init__ = lambda i,**d: i.__dict__.update(d)
  __repr__ = lambda i    : i.__class__.__name__+"("+dict2str(i.__dict__)+")"
# -------------------------------------------------------------------
#                     _         
#      _   _   ._   _|_  o   _  
#     (_  (_)  | |   |   |  (_| 
#                            _|     
the = o(
  seed  = 1234567891, 
  round = 2,
  train = "data/misc/auto93.csv", 
  bins  = o(max    = 17,
            enough = 0.5),
  bayes = o(m=2,
            k=1,
            label=4,
            Last=30,
            any=100,
            best=0.5))                                                    
# ------------------------------------------------------------------- 
#     ._        ._ _    _      _      ._ _    _ 
#     | |  |_|  | | |  _>     _>  \/  | | |  _> 
#              
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
#      _|   _.  _|_   _.     _  _|_   _   ._   _.   _    _  
#     (_|  (_|   |_  (_|    _>   |_  (_)  |   (_|  (_|  (/_ 
#                                                   _|      
class DATA(o):
  def __init__(i)      : i.rows=[];  i.cols = o(all=[],x=[],y=[],names=[])
  def add(i,row)       : (i.data if i.cols.all else i.head)(row)
  def clone(i,rows=[]) : return DATA().fromList([i.cols.names] + rows)
  def chebyshev(i,row) : return max(abs(c.goal - c.norm(row[c.at])) for c in i.cols.y)
  def fromFile(i,file) : [i.add(row) for row in csv(file)]; return i
  def fromList(i,lst)  : [i.add(row) for row in lst      ]; return i 
  def sort(i)          : i.rows.sort(key = i.chebyshev)   ; return i
q
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
#     |_   o  ._  
#     |_)  |  | | 

class BIN(o):
  def __init__(i,txt, at,lo, hi,  n=0,  ymid=0, ydiv=0):
    i.txt,i.at,i.lo,i.hi,i.ymid,i.ydiv = txt,at,lo,hi or lo,ymid,ydiv
    i.n, i.yhelper = n, NUM()

  def __repr__(i):
    s,lo,hi = i.txt, i.lo, i.hi
    if lo == -1E32: return f"{s} <= {hi}"
    if hi ==  1E32: return f"{s} > {lo}"
    if lo==hi     : return f"{s} = {lo}"
    return f"{lo} < {s} <= {hi}"

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

def bins2bin(bins):
  "Combine N bins into one"
  if bins==[]: return []
  n, ymids, ydivs, lo, hi = 0, 0, 0, bins[0].lo, bins[0].hi
  for b in bins:
    n      = int(n + b.n)
    lo,hi  = min(lo, b.lo), max(hi, b.hi)
    ymids += b.ymid * b.n
    ydivs += b.ydiv * b.n
  return BIN(bins[0].txt, bins[0].at, lo, hi, n=n, ymid=ymids/n, ydiv=ydivs/n)

def makeBins(col, rows, y, enough):
  out={}
  for row in sorted(rows,key=lambda r: -1E32 if r[col.at]=="?" else r[col.at]):
      x=row[col.at]
      if x != "?":
        b = col.bin(x) ;   
        out[b] = out.get(b,None) or BIN(col.txt,col.at,x,x)
        out[b].add(x, y(row)) 
  out = mergeBins(col, enough, sorted(out.values(), key=lambda b:b.lo)) 
  return out if out else []

def mergeBins(col, enough, bins):
  "return two bins that give the most reduction in overall y-diversity"
  if not isNum(col): return bins
  least, out = 1E32 , None
  #b4 = bins2bin(bins) 
  for j in range(1,len(bins)):
    one, two = bins2bin(bins[:j]), bins2bin(bins[j:]) 
    here = one.n * one.ydiv  + two.n * two.ydiv 
    if here  < least and one.n > enough and two.n > enough:
      least, out = here, [one, two]
      one.lo, two.hi = -1E32, 1E32
      two.lo = one.hi
  return out
# ---------------------------------------------------------------------------------------
#     _|_  ._   _    _  
#      |_  |   (/_  (/_ 

class TREE(o):
  def __init__(i,data,here,lvl,bin=None):
    i.data,i.here, i.lvl, i.bin, i.kids = data, here, lvl, bin, []
    if  bin:
      i.ymid = bin.ymid
    else:
      n=NUM()
      [n.add(data.chebyshev(row)) for row in i.here.rows]
      i.ymid = n.mu 

  def __repr__(i):
    return f"{i.ymid:.2f} {len(i.here.rows):5}  {'|.. '*(i.lvl-1)} {'' if i.lvl==0 else i.bin}"

  def nodes(i):
    yield i 
    for kid in i.kids: 
      for sub in kid.nodes(): yield sub 

def tree(data,rows=None, stop=None):
  def grow(rows, stop=None, lvl=0, above=None):
    stop = stop or len(rows)**0.5
    tree = TREE(data,data.clone(rows), lvl, above)  
    for bin in bestSplitter(data,rows): 
      sub = bin.selects(rows) 
      if len(sub) < len(rows) and len(sub) > stop: 
        tree.kids.append(grow(sub, stop=stop, lvl=lvl+1, above=bin))
    return tree
  return grow(rows or data.rows)
  
def bestSplitter(data,rows):
  out, least = [], 1E32, 
  for col in data.cols.x:
    bins = [b for b in makeBins(col, rows, data.chebyshev,len(data.rows)**the.bins.enough)] 
    tmp  = bins2bin(bins)  
    if len(bins) > 0 and tmp.ydiv < least:
      least = tmp.ydiv 
      out   = bins 
  return sorted(out, key=lambda b:b.ymid) 
# ---------------------------------------------------------------------------------------
#     |_    _.       _    _ 
#     |_)  (_|  \/  (/_  _> 
#               /           

def loglikes(data, row, nall, nh):
  prior = (len(data.rows) + the.bayes.k) / (nall + the.bayes.k*nh)
  likes = [like(col, row[col.at], prior) for col in data.cols.x if row[col.at] != "?"]
  return sum(log(x) for x in likes + [prior] if x>0)

def like(col, x, prior) :
  if not isNum(col):
    return (col.has.get(x,0) + the.bayes.m*prior) / (col.n+the.bayes.m)
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
      done = data.clone(done).sort().rows
    return done

  random.shuffle(data.rows) # remove any  bias from older runs
  return smo1(data.rows[the.bayes.label:],
              data.clone(data.rows[:the.bayes.label]).sort().rows)
# ---------------------------------------------------------------------------------------
#      _  _|_   _.  _|_   _ 
#     _>   |_  (_|   |_  _> 
                          
class SOME:
    "Non-parametric statistics using reservoir sampling."
    def __init__(i, inits=[], txt="", max=512): 
      "Start stats. Maybe initialized with `inits`. Keep no more than `max` numbers."
      i.txt,i.max,i.lo, i.hi  = txt,max, 1E30, -1E30
      i.rank,i.n,i._has,i.ok = 0,0,[],True
      i.adds(inits)  

    def __repr__(i): 
      "Print the reservoir sampling."
      return  'SOME('+str(dict(txt=i.txt,rank="i.rank",n=i.n,all=len(i._has),ok=i.ok))+")"

    def adds(i,a):  
      "Handle multiple nests samples."
      for b in a:
        if   isinstance(b,(list,tuple)): [i.adds(c) for c in b]  
        elif isinstance(b,SOME):         [i.add(c) for c in b._has]
        else: i.add(b) 

    def add(i,x):  
      i.n += 1
      i.lo = min(x,i.lo)
      i.hi = max(x,i.hi)
      now  = len(i._has)
      if   now < i.max   : i.ok=False; i._has += [x]
      elif R() <= now/i.n: i.ok=False; i._has[ int(R() * now) ]

    def __eq__(i,j):
      "True if all of cohen/cliffs/bootstrap say you are the same."
      return i.cliffs(j) and i.bootstrap(j) ## ordered slowest to fastest

    def has(i) :
      "Return the numbers, sorted."
      if not i.ok: i._has.sort()
      i.ok=True
      return i._has

    def mid(i):
      "Return the middle of the distribution."
      l = i.has(); return l[len(l)//2]

    def div(i):
       "Return the deviance from the middle." 
       l = i.has(); return (l[9*len(l)//10] - l[len(l)//10])/2.56

    def pooledSd(i,j):
      "Return a measure of the combined standard deviation."
      sd1, sd2 = i.div(), j.div()
      return (((i.n - 1)*sd1 * sd1 + (j.n-1)*sd2 * sd2) / (i.n + j.n-2))**.5

    def norm(i, n):
      "Noramlize `n` to the range 0..1 for min..max"
      return (n-i.lo)/(i.hi - i.lo + 1E-30)

    def bar(i, some, fmt="%8.3f", word="%10s", width=50):
      "Pretty print `some.has`."
      has = some.has() 
      out = [' '] * width
      cap = lambda x: 1 if x > 1 else (0 if x<0 else x)
      pos = lambda x: int(width * cap(i.norm(x)))
      [a, b, c, d, e]  = [has[int(len(has)*x)] for x in [0.1,0.3,0.5,0.7,0.9]]
      [na,nb,nc,nd,ne] = [pos(x) for x in [a,b,c,d,e]] 
      for j in range(na,nb): out[j] = "-"
      for j in range(nd,ne): out[j] = "-"
      out[width//2] = "|"
      out[nc] = "*" 
      return ', '.join(["%2d" % some.rank, word % some.txt, fmt%c, fmt%(d-b),
                        ''.join(out),fmt%has[0],fmt%has[-1]])

    def delta(i,j):
      "Report distance between two SOMEs, modulated in terms of the standard deviation."
      return abs(i.mid() - j.mid()) / ((i.div()**2/i.n + j.div()**2/j.n)**.5 + 1E-30)

    def cohen(i,j):
      return abs( i.mid() - j.mid() ) < the.cohen * i.pooledSd(j)

    def cliffs(i,j, dull=0.147):
      """non-parametric effect size. threshold is border between small=.11 and medium=.28 
      from Table1 of  https://doi.org/10.3102/10769986025002101"""
      n,lt,gt = 0,0,0
      for x1 in i.has():
        for y1 in j.has():
          n += 1
          if x1 > y1: gt += 1
          if x1 < y1: lt += 1
      return abs(lt - gt)/n  < dull  

    def  bootstrap(i,j,confidence=.05,samples=512):
      """non-parametric significance test From Introduction to Bootstrap, 
        Efron and Tibshirani, 1993, chapter 20. https://doi.org/10.1201/9780429246593"""
      y0,z0  = i.has(), j.has()
      x,y,z  = SOME(inits=y0+z0), SOME(inits=y0), SOME(inits=z0)
      delta0 = y.delta(z)
      yhat   = [y1 - y.mid() + x.mid() for y1 in y0]
      zhat   = [z1 - z.mid() + x.mid() for z1 in z0] 
      pull   = lambda l:SOME(random.choices(l, k=len(l))) 
      n      = sum(pull(yhat).delta(pull(zhat)) > delta0 for _ in range(samples)) 
      return n / samples >= confidence
# ---------------------------------------------------------------------------------------
#      _  _|_   _.  _|_   _         _|_  o  |   _ 
#     _>   |_  (_|   |_  _>    |_|   |_  |  |  _>                                               

def sk(somes):
  "Sort nums on mid. give adjacent nums the same rank if they are statistically the same"
  def sk1(somes, rank, cut=None):
    most, b4 = -1, SOME(somes)
    for j in range(1,len(somes)):
      lhs = SOME(somes[:j])
      rhs = SOME(somes[j:])
      tmp = (lhs.n*abs(lhs.mid() - b4.mid()) + rhs.n*abs(rhs.mid() - b4.mid())) / b4.n
      if tmp > most:
         most,cut = tmp,j
    if cut:
      some1,some2 = SOME(somes[:cut]), SOME(somes[cut:])
      if not some1.cohen(some2):
        if some1 != some2:
          rank = sk1(somes[:cut], rank) + 1
          rank = sk1(somes[cut:], rank)
          return rank
    for some in somes: some.rank = rank
    return rank
  somes = sorted(somes, key=lambda some: some.mid()) #lambda some : some.mid())
  sk1(somes,0)
  return somes

def file2somes(file):
  "Reads text file into a list of `SOMEs`."
  def asNum(s):
    try: return float(s)
    except Exception: return s
  somes=[]
  with open(file) as fp: 
    for word in [asNum(x) for s in fp.readlines() for x in s.split()]:
      if isinstance(word,str): some = SOME(txt=word); somes.append(some)
      else                   : some.add(word)    
  return somes

def bars(somes, width=40):
  "Prints multiple `somes` on the same scale."
  all = SOME(somes)
  last = None
  for some in sk(some):
    if some.rank != last: print("#")
    last=some.rank
    print(all.bar(some.has(), width=width, word="%20s", fmt="%5.2f"))
# ---------------------------------------------------------------------------------------
#          _|_  o  |   _ 
#     |_|   |_  |  |  _>                       

def isNum(col): return isinstance(col,NUM)

def dict2str(d):
  rnd = lambda x: round(x,the.round) if isinstance(x,float) else x
  return ", ".join([f"{k}={rnd(v)}" for k,v in d.items()])

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
#      _    _    _ 
#     (/_  (_|  _> 
#           _|     

class eg:
  def egs(_):
    ":show all examples"
    for s in dir(eg): 
      if s[0] != "_":
        doc = getattr(eg,s).__doc__ or f"[ARG]:set {s}"
        a = doc.split(":")
        print(f"  -{s:5} {a[0]}\t  {a[1]} ")

  def h(_): 
    ":show help"
    print("ezr.py -[h|seed|egs|OTHERS] [ARG]")
    print("ezr.py -egs (to list all the OTHER actions)")

  def k(n)   : the.bayes.k = n
  def m(n)   : the.bayes.m = n
  def seed(n): the.seed    = n; random.seed(n); 

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
        print(o(bin=bin,n=bin.n,ymid=bin.ymid))

  def tree(file):
    "[FILE]:test bin generation"
    d = DATA().fromFile(file or the.train)
    t = tree(d, d.rows, stop=10)
    for node in t.nodes(): print(node)

  def smo(file):
    "[FILE]:test bin generation"
    d= DATA().fromFile(file or the.train)
    print(sorted([round(d.chebyshev(smo(d)[0]),2) for i in range(20)]))

def main(a):
  random.seed(the.seed)
  for i,arg in enumerate(a): 
    if len(arg) > 1: 
      fun = getattr(eg, arg[1:] , None)
      if fun:  
        fun( coerce(a[i+1]) if i < len(a)-1 else the.train )

if __name__ == "__main__" and len(sys.argv) > 1:  main(sys.argv)#11
aa