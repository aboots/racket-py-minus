def f(n = 0):
    a = n
    b = n + 1
    return a ** b

def g():
    c = 1 < 7 and 13 > 17 or 1 == 1
    print(1 < 7 and 13 > 17 or 1 == 1)
    return c

l = [1, 3, 5, 7]

if g():
    h = f
    print(h(3))
else:
    print(0)

if False:
    print(l[2])
else:
    print(l[3])