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
Loki.printf([])
Loki.printf([])
Loki.printf([1, 2, 3])
Loki.printf([4, 5, 6])
Loki.printf([7, 8, 9])
Loki.printf({})
Loki.printf({})
Loki.printf({"x" : 1})
Loki.printf({"z" : 3, "y" : 2})
Loki.printf({"c" : 6, "b" : 5, "a" : 4})
Loki.printf({"e" : 8, "d" : 7})
x = Loki.plus(2, 3)
Loki.printf((1,"2",(2,x)))