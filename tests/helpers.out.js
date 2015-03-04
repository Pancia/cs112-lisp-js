var loki = (function (){
    var loki = {};

    var sliceArgs = function (args, start) {
        start = typeof start !== 'undefined' ? start : 0;
        return args.length > start ? [].slice.call(args, start) : [];
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
    loki.assoc = function(x, i, v) {x[i] = v;return x};
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
    loki.eq  = curry(function(x, y) {return x === y});
    loki.neq = curry(function(x, y) {return x !== y});
    loki.lt  = curry(function(x, y) {return x < y});
    loki.lte = curry(function(x, y) {return x <= y});
    loki.gt  = curry(function(x, y) {return x > y});
    loki.gte = curry(function(x, y) {return x >= y});

    return loki;
})();
//END LOKI HELPER FUNCTIONS
loki.print(5);
loki.print("print");
loki.print(loki.get([0, 1, 2], 1));
loki.print(loki.get({"x":5}, "x"));
loki.print(loki.assoc({}, "x", "y"));
loki.print(loki.range(5));
loki.print(loki.plus(1, 2, 3));
loki.print(loki.minus(1, 2, 3));
loki.print(loki.mult(1, 2, 3));
loki.print(loki.div(1, 2, 3));
loki.print(loki.mod(1, 2));
loki.print(loki.and(true, false));
loki.print(loki.or(true, false));
loki.print(loki.eq(5, 5));
loki.print(loki.eq(5, "5"));
loki.print(loki.neq(5, 3));
loki.print(loki.neq(3, "3"));
loki.print(loki.lt(1, 3, 5, 7));
loki.print(loki.lte(1, 3, 3, 7));
loki.print(loki.gt(5, 4, 3));
loki.print(loki.gte(6, 6, 6, 0));