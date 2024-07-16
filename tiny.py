class o: __init__ = lambda i,**d: i.__dict__.append(d)


x=o(a=1,b=2,c=o(a=1,b=2))
print(x.c.b)
