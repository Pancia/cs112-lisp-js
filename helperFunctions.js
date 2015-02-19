load("functional.js");

//var className = {type:"anything", someFunc: function(c){print (c);}};

var log = function() {
    var _log = function(x) {print(x)}
    var args = Array.prototype.slice.call(arguments);
    args.forEach(_log);
};

var get_ = function(e, i){
	return e[i];
};

var plus  = fjs.curry(function(x, y) {return x + y});
var minus = fjs.curry(function(x, y) {return x - y});
var mult  = fjs.curry(function(x, y) {return x * y});
var div   = fjs.curry(function(x, y) {return x / y});

var and = fjs.curry(function(x, y) {return x && y});
var or  = fjs.curry(function(x, y) {return x || y});
var eq  = fjs.curry(function(x, y) {return x == y});
var neq = fjs.curry(function(x, y) {return x != y});
var lt  = fjs.curry(function(x, y) {return x < y});
var lte = fjs.curry(function(x, y) {return x <= y});
var gt  = fjs.curry(function(x, y) {return x > y});
var gte = fjs.curry(function(x, y) {return x >= y});
//END HELPER FUNCTIONS
