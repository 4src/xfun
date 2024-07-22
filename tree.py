class o:
  __init__ = lambda i,**d : i.__dict__.update(d)
  __repr__ = lambda i     : i.__class__.__name__+"("+str(i.__dict__)+")"

the = o(train="data/misc/config")

def coerce(s):
  try: return ast.literal_eval(s)
  except Exception:  return s

def csv(file="-"):
  with file_or_stdin(None if file=="-" else file) as src:
    for line in src:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line: yield [coerce(s.strip()) for s in line.split(",")]

data = o(rows=[], cols=o(y={}, nums={}, names={}))
for i,row in enumerate(csv(the.train)):
  if i==0: 
    data.cols.names = row
    for j,s in enumerate(row): 
      if s[-1] != "X":
        if s[0].isupper(): data.col.nums[j] = o(at=j, txt=s, lo=1E30, hi=-1E30) 
        if s[-1] in "!+-": data.col.y[j]
  else:
    for num in i.data.col.nums: 
      if row[num.at] != "?":
        num.lo = min(num.lo, row[num.at])
        num.hi = max(num.hi, row[num.at])

def norm(data,c,x):
  if x=="?" or c not in data.cols.nums: return x
  lo,hi = data.cols.nums[c].lo, data.cols.nums[c].hi
  return (x - lo)/ (hi - lo + 1E-32)

def dist(data,c,x,y):
  if x==y=="?": return 1
  if num := data.cols.nums.get(c,None):
    x, y = norm(data,c,x), norm(data,c,y)
    x = x if x !="?" else (1 if y<0.5 else 0)
    y = y if y !="?" else (1 if x<0.5 else 0)
    return abs(x-y)
  else: return x


    r

    