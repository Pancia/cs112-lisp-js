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
Loki.printf("TODO QUOTED")

Loki.printf("py")

Loki.printf("print this!") if Loki.lt(1, 2) else Loki.printf("do not print this!")
x = 5
Loki.printf("this is wrong") if Loki.gt(x, 10) else Loki.printf("this is right")
for x in Loki.range(3):
    Loki.printf(x)
for x in Loki.range(3):
    Loki.printf("x")
    Loki.printf(x)
    for y in Loki.range(3):
        Loki.printf("y")
        Loki.printf(y)
Loki.printf(x)
x = 7
Loki.printf(x)
class Rocket():
    def __init__(self, x):
        self.speed = x

    color = "red"
    fuel = 7
    def lift_off (self):
        return Loki.printf(Loki.plus("I'm flying @ ", self.speed() if callable(self.speed) else self.speed, " speed"))

    def toString (self):
        return Loki.plus("I'm a ", self.color() if callable(self.color) else self.color, " rocket")


r = Rocket(7)
Loki.printf(r.speed() if callable(r.speed) else r.speed)
r.speed = 10
Loki.printf(r.speed() if callable(r.speed) else r.speed)