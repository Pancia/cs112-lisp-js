class Loki:
    @staticmethod
    def printf(x):
        print(x)
    @staticmethod
    def plus(*args):
        return reduce((lambda x, y : x + y), args)
    @staticmethod
    def minus(*args):
        return reduce((lambda x, y : x - y), args)
    @staticmethod
    def div(*args):
        return reduce((lambda x, y : x / y), args)
    @staticmethod
    def mult(*args):
        return reduce((lambda x, y : x * y), args)
    @staticmethod
    def and_(*args):
        return reduce((lambda x, y : x and y), args)
    @staticmethod
    def or_(*args):
        return reduce((lambda x, y : x or y), args)
    @staticmethod
    def eq(*args):
        return reduce((lambda x, y : x == y), args)
    @staticmethod
    def neq(*args):
        return reduce((lambda x, y : x != y), args)
    @staticmethod
    def lt(*args):
        return reduce((lambda x, y : x < y), args)
    @staticmethod
    def lte(*args):
        return reduce((lambda x, y : x <= y), args)
    @staticmethod
    def gt(*args):
        return reduce((lambda x, y : x > y), args)
    @staticmethod
    def gte(*args):
        return reduce((lambda x, y : x >= y), args)
    @staticmethod
    def mod(x, y):
        return x % y
    @staticmethod
    def range(n):
        return range(n)
    @staticmethod
    def get(e, i):
        return e[i]
    @staticmethod
    def set(x, v):
        x = v
    @staticmethod
    def assoc(x, i, v):
        x[i] = v

#END LOKI HELPER FUNCTIONS
x = None
y = 2
x = y
x = Loki.plus(1, 2)
y = [1, 2]
x = None
x = None
x = None
x = 1
x = 1
x = 1
def y (x):
    return Loki.printf(x)
def z ():
    return Loki.printf("hello")
def a ():
    return []
def y (x):
    return Loki.printf(x)
def y (x):
    return Loki.printf(x)
def y (x):
    return Loki.printf(x)
Loki.printf((lambda  : [[]])())
Loki.printf((lambda x : [[]])(1))
Loki.printf((lambda x : [Loki.printf(x)])(4))
Loki.printf((lambda x, y : [[]])(2, 5))
Loki.printf((lambda x, y : [Loki.printf(Loki.plus(x, y))])(1, 9))
Loki.printf((lambda x, y, z : [Loki.printf("first"), Loki.printf(Loki.plus(x, y, z))])(1, 1, 1))
Loki.printf((lambda x : [Loki.printf(x)])(7))
Loki.printf((lambda x : [Loki.printf(x)])(12))