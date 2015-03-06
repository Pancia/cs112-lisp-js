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

    loki.extend = function (destination, source) {
        for (var k in source) {
            if (source.hasOwnProperty(k)) {
                destination[k] = source[k];
            }
        }
        return destination;
    }

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
var x;
var y = 2;
var x = y;
var x = loki.plus(1, 2);
var y = [1, 2];
var x;
var x;
var x;
var x = 1;
var x = 1;
var x = 1;
var y = function (x) {
return loki.print(x)
};
var z = function () {
return loki.print("hello")
};
var a = function () {
return []
};
var y = function (x) {
return loki.print(x)
};
var y = function (x) {
return loki.print(x)
};
var y = function (x) {
return loki.print(x)
};
loki.print(function () {
return []
}());
loki.print(function (x) {
return []
}(1));
loki.print(function (x) {
return loki.print(x)
}(4));
loki.print(function (x, y) {
return []
}(2, 5));
loki.print(function (x, y) {
return loki.print(loki.plus(x, y))
}(1, 9));
loki.print(function (x, y, z) {
loki.print("first");
return loki.print(loki.plus(x, y, z))
}(1, 1, 1));
loki.print(function (x) {
return loki.print(x)
}(7));
loki.print(function (x) {
return loki.print(x)
}(12));