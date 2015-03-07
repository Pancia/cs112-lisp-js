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
        return not not reduce((lambda x, y : x if x == y else False), args)
    @staticmethod
    def neq(*args):
        return not Loki.eq(*args)
    @staticmethod
    def lt(*args):
        return not not reduce((lambda x, y : y if x < y else False), args)
    @staticmethod
    def lte(*args):
        return not not reduce((lambda x, y : y if x <= y else False), args)
    @staticmethod
    def gt(*args):
        return not not reduce((lambda x, y : y if x > y else False), args)
    @staticmethod
    def gte(*args):
        return not Loki.lt(*args)
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
        return x
    @staticmethod
    def in_(x, l):
        return (x in l)
    @staticmethod
    def sc(n, x, l):
        return n[x:l]
    @staticmethod
    def dc(n, x, l):
        return n[x::l]
    @staticmethod
    def dcm(n, x, m, l):
        return n[x:m:l]
    @staticmethod
    def not_ (x):
        return not x

#END LOKI HELPER FUNCTIONS
Loki.printf(5)
Loki.printf("print")
Loki.printf(Loki.get([0, 1, 2], 1))
Loki.printf(Loki.get({"x" : 5}, "x"))
Loki.printf(Loki.assoc({}, "x", "y"))
Loki.printf(Loki.range(5))
Loki.printf(Loki.plus(1, 2, 3))
Loki.printf(Loki.minus(1, 2, 3))
Loki.printf(Loki.mult(1, 2, 3))
Loki.printf(Loki.div(1, 2, 3))
Loki.printf(Loki.mod(1, 2))
Loki.printf(Loki.and_(True, False))
Loki.printf(Loki.or_(True, False))
Loki.printf(Loki.eq(5, 3))
Loki.printf(Loki.eq(3, "3"))
Loki.printf(Loki.neq(5, 3))
Loki.printf(Loki.neq(3, "3"))
Loki.printf(Loki.lt(1, 3, 5, 7))
Loki.printf(Loki.lte(1, 3, 3, 7))
Loki.printf(Loki.gt(5, 4, 3))
Loki.printf(Loki.gte(6, 6, 6, 0))
r = Loki.range(20)
Loki.printf(Loki.sc(r, 1, 3))
Loki.printf(Loki.dc(r, 1, 2))
Loki.printf(Loki.dcm(r, 1, 2, 7))
Loki.printf(Loki.in_("h", "hello"))
Loki.printf(Loki.in_("f", "hello"))
Loki.printf(Loki.not_(Loki.in_("f", "hello")))
Loki.printf(Loki.not_(Loki.in_("h", "hello")))