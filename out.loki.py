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
    def get(e, i):
        return e[i]
    @staticmethod
    def set(x, v):
        x = v
    @staticmethod
    def assoc(x, i, v):
        x[i] = v

#END HELPER FUNCTIONS
[(lambda  : 7)]