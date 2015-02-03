load("functional.js");

var plus = fjs.curry(function(x, y) {return x + y});
var minus = fjs.curry(function(x, y) {return x - y});
var mult = fjs.curry(function(x, y) {return x * y});
// TODO: Add div and mult

var log = function() {
    var _log = function(x) {print(x)}
    var args = Array.prototype.slice.call(arguments);
    args.forEach(_log);
}

var and = fjs.curry(function(x, y) {return x && y});
var or  = fjs.curry(function(x, y) {return x || y});
var eq  = fjs.curry(function(x, y) {return x == y});
var neq = fjs.curry(function(x, y) {return x != y});

//END HELPER FUNCTIONS
