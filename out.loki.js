var loki = (function (){
    var loki = {};

    var sliceArgs = function (args) {
        return args.length > 0 ? [].slice.call(args, 0) : [];
    };

    var assertIsFunction = function (f) {
        if (typeof f !== "function")
        {throw "loki error: curry expected a function"}
        return f;
    };

    var curry = function (fn) {
        assertIsFunction(fn);
        return function inner() {
            var _args = sliceArgs(arguments);
            if (_args.length === fn.length) {
                return fn.apply(null, _args);
            } else if (_args.length > fn.length) {
                var initial = fn.apply(null, _args);
                return foldl(fn, initial, _args.slice(fn.length));
            } else {
                return function() {
                    var args = sliceArgs(arguments);
                    return inner.apply(null, _args.concat(args));
                };
            }
        };
    };

    var each = curry(function (iterator, items) {
        assertIsFunction(iterator);
        if (items == null || !Array.isArray(items)) {return;}
        items.forEach(function (e, i) {iterator.call(null, e, i)});
    });

    var foldl = curry(function (iterator, acc, xs) {
        assertIsFunction(iterator);
        each(function (x, i) {
            acc = iterator.call(null, acc, x, i);
        }, xs);
        return acc;
    });

    loki.print = function() {
        var _log = function(x) {
            if (typeof console === "object") {console.log(x);}
            else {assertIsFunction(print)(x);}
        };
        var args = sliceArgs(arguments);
        args.forEach(_log);
    };
    loki.get = function(e, i) {return e[i];};
    loki.set = function(x, v) {x = v;};
    loki.assoc = function(x, i, v) {x[i] = v;};
    loki.range = function(N) {return Array.apply(null, {length: N}).map(Number.call, Number);};

    //Arithmetic
    loki.plus  = curry(function(x, y) {return x + y});
    loki.minus = curry(function(x, y) {return x - y});
    loki.mult  = curry(function(x, y) {return x * y});
    loki.div   = curry(function(x, y) {return x / y});
    loki.mod   = curry(function(x, y) {return x % y});

    //Logic
    loki.and = curry(function(x, y) {return x && y});
    loki.or  = curry(function(x, y) {return x || y});
    loki.eq  = curry(function(x, y) {return x == y});
    loki.neq = curry(function(x, y) {return x != y});
    loki.lt  = curry(function(x, y) {return x < y});
    loki.lte = curry(function(x, y) {return x <= y});
    loki.gt  = curry(function(x, y) {return x > y});
    loki.gte = curry(function(x, y) {return x >= y});

    return loki;
})();
//END LOKI HELPER FUNCTIONS
var painted;
var content;
var winningCombos;
var turn = 0;
var theCanvas;
var c;
var cxt;
var squaresFilled = 0;
var w;
var y;
myObj = {};
myObj.foo = 5;
loki.print(myObj);
window.onload = function () {
painted = [];
content = [];
winningCombos = [[0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7], [2, 5, 8], [0, 4, 8], [2, 4, 6]];
return loki.range(9).forEach(function () {
painted = false;
return content = ""
})
};
var t1 = function (cn) {
cxt.beginPath;
cxt.moveTo;
cxt.lineTo;
cxt.moveTo;
cxt.lineTo;
cxt.stroke;
cxt.closePath;
return loki.assoc(content, loki.minus(cn, 1), "X")
};
var t2 = function (cn) {
cxt.beginPath;
cxt.arc(25, 25, 20, 0, loki.mult(3, 2), true);
cxt.stroke;
cxt.closePath;
return loki.assoc(content, loki.minus(cn, 1), "O")
};
var t3 = function (cn) {
turn = loki.plus(turn, 1);
loki.assoc(painted, loki.minus(cn, 1), true);
squaresFilled = loki.plus(squaresFilled, 1);
checkForWinners(loki.get(content, loki.minus(cn, 1)));
(loki.eq(squaresFilled, 9)?alert("Draw!"):location.reload(true));
return alert("Invalid move!")
};
var canvasClicked = function (cn) {
theCanvas = loki.plus("canvas", cn);
c = document.getElementById(theCanvas);
cxt = c.getContext("2d");
return (loki.eq(loki.get(painted, loki.minus(cn, 1)), false)?(loki.eq(loki.mod(turn, 2))?t1(cn):t2(cn)):t3(cn))
};
var t4 = function () {
alert(loki.plus(symbol, " won!"));
return playAgain()
};
var checkForWinners = function (symbol) {
return loki.range(6).forEach(function () {
return (and(loki.eq(loki.get(content, loki.get(winningCombos, a, loki.get(winningCombos, 0))), symbol), loki.eq(loki.get(content, loki.get(winningCombos, a, loki.get(winningCombos, 1))), symbol), loki.eq(loki.get(content, loki.get(winningCombos, a, loki.get(winningCombos, 2))), symbol))?t4():null)
})
};
var playAgain = function () {
y = confirm("Play again?");
return (loki.eq(y, true)?location.reload(true):null)
};