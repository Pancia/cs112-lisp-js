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

#END HELPER FUNCTIONS
