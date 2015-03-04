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
Loki.printf(Loki.eq(5, 5))
Loki.printf(Loki.eq(5, "5"))
Loki.printf(Loki.neq(5, 3))
Loki.printf(Loki.neq(3, "3"))
Loki.printf(Loki.lt(1, 3, 5, 7))
Loki.printf(Loki.lte(1, 3, 3, 7))
Loki.printf(Loki.gt(5, 4, 3))
Loki.printf(Loki.gte(6, 6, 6, 0))