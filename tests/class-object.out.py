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
class MyObject():
    def __init__(self, x):
        self.obj = x

    def get_obj (self):
        return self.obj() if callable(self.obj) else self.obj


class MyThing():
    def __init__(self, x):
        self.thing = x

    def get_thing (self):
        return self.thing() if callable(self.thing) else self.thing


class Rocket(MyObject, MyThing):
    def __init__(self, x):
        MyObject.__init__(self,"obj")
        MyThing.__init__(self,"thing")
        self.speed = x
        Loki.printf("eval-in-constr")
        self.foo = "foo"
        for x in Loki.range(10):
            Loki.printf(x)
    color = "red"
    fuel = 7
    def lift_off (self):
        return Loki.printf(Loki.plus("I'm flying @ ", self.speed() if callable(self.speed) else self.speed, " speed"))

    def toString (self):
        return Loki.plus("I'm a ", self.color() if callable(self.color) else self.color, " rocket")


r = Rocket("5")
Loki.printf(r.foo() if callable(r.foo) else r.foo)
Loki.printf(r.color() if callable(r.color) else r.color)
Loki.printf(r.speed() if callable(r.speed) else r.speed)
Loki.printf(r.toString() if callable(r.toString) else r.toString)
Loki.printf(r.fuel() if callable(r.fuel) else r.fuel)
r.lift_off() if callable(r.lift_off) else r.lift_off
Loki.printf(r.get_obj() if callable(r.get_obj) else r.get_obj)
Loki.printf(r.thing() if callable(r.thing) else r.thing)