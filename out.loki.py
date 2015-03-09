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
    def __init__(self):
        self.obj = 5

    def get_object (self): 
        return "object" 


o = MyObject()
Loki.printf(o.get_object() if callable(o.get_object) else o.get_object)
class Thing():
    def __init__(self):
        self.thing = 5

    def get_thing (self): 
        return "thing" 


t = Thing()
Loki.printf(t.get_thing() if callable(t.get_thing) else t.get_thing)
class Rocket(MyObject, Thing):
    def __init__(self, x):
        self.p = x
        self.f = 0

    speed = 5
    def fe (self): 
        return Loki.printf("ehhhh") 

    def funct (self, x, y): 
        return Loki.printf(Loki.plus(x, y)) 


r = Rocket(5)
Loki.printf(r.f() if callable(r.f) else r.f)
Loki.printf(r.get_object() if callable(r.get_object) else r.get_object)
Loki.printf(r.get_thing() if callable(r.get_thing) else r.get_thing)
for lskdfjk in Loki.range(2):
    for xyz in Loki.range(4):
        for abc in Loki.range(7):
            for lskdfjl in Loki.range(2):
                Loki.printf(xyz)