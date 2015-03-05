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
        self.speed = x

    color = "red"
    fuel = 5
    def lift_off (self): 
        return Loki.printf(Loki.plus("I'm flying @ ", self.speed() if callable(self.speed) else self.speed, " speed")) 

    def toString (self): 
        return Loki.plus("I'm a ", self.color() if callable(self.color) else self.color, " rocket") 


r = Rocket("5")
Loki.printf(r.color() if callable(r.color) else r.color)
Loki.printf(r.speed() if callable(r.speed) else r.speed)
Loki.printf(r.toString() if callable(r.toString) else r.toString)
Loki.printf(r.fuel() if callable(r.fuel) else r.fuel)
r.lift_off() if callable(r.lift_off) else r.lift_off