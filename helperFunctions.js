var log = function() {
    var _log = function(x) {if (console != undefined) {console.log(x)} else {print(x)}}
    var args = Array.prototype.slice.call(arguments);
    args.forEach(_log);
};

if (load != undefined) {load("functional.js");}
else {log("WARNING: not running in a valid cmdl env,"+
          "use JSC or make sure to import necessary libraries");}

var get = function(e, i){
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
