def plus (l):
	return reduce((lambda x, y : x + y), l)

def minus (l):
	return reduce((lambda x, y : x - y), l)

def div (l):
	return reduce((lambda x, y : x / y), l)

def mult (l):
	return reduce((lambda x, y : x * y), l)

def and_ (l):
	return reduce((lambda x, y : x and y), l)

def or_ (l):
	return reduce((lambda x, y : x or y), l)

def eq (l):
	return reduce((lambda x, y : x == y), l)

def neq (l):
	return reduce((lambda x, y : x != y), l)

def lt (l):
	return reduce((lambda x, y : x < y), l)

def lte (l):
	return reduce((lambda x, y : x <= y), l)

def gt (l):
	return reduce((lambda x, y : x > y), l)

def gte (l):
	return reduce((lambda x, y : x >= y), l)

def get (e, i):
	return e[i]

class Dog:
    tricks = []             # mistaken use of a class variable
    def __init__(self, name):
        self.name = name
    def add_trick(self, trick):
        self.tricks.append(trick)

d = Dog('Fido')
#END HELPER FUNCTIONS
