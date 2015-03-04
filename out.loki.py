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
class Rocket:
    def __init__(self, x):
        self.p = x
        self.f = 0

    speed = 5
    def fe (self): 
        return Loki.printf("ehhhh") 

    def funct (self, x, y): 
        return Loki.printf(Loki.plus(x, y)) 


r = Rocket(5)
r.funct(4, 2) if callable(r.funct) else r.funct
r.fe() if callable(r.fe) else r.fe
Loki.printf(r.speed() if callable(r.speed) else r.speed)