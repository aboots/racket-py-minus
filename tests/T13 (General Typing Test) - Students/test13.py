def f(n: int = 0) -> int:
    a: int = n
    b: int = n + 1
    return a ** b

def g() -> bool:
    c :bool = 1 < 7 and 13 > 17 or 1 == 1
    print(1 < 7 and 13 > 17 or 1 == 1)
    return c

l: list = [1, 3, 5, 7]

if g():
    print(l[3])
else:
    print(l[2])
