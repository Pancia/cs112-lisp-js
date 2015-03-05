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
function Rocket(x) {
this.color = "red";
this.fuel = 5;
this.speed = x
};
Rocket.prototype.lift_off = function() {
 return loki.print(loki.plus("I'm flying @ ", (typeof this.speed === "function" ? this.speed() : this.speed), " speed"))
};
Rocket.prototype.toString = function() {
 return loki.plus("I'm a ", (typeof this.color === "function" ? this.color() : this.color), " rocket")
};;
var r = new Rocket("5");
loki.print((typeof r.color === "function" ? r.color() : r.color));
loki.print((typeof r.speed === "function" ? r.speed() : r.speed));
loki.print((typeof r.toString === "function" ? r.toString() : r.toString));
loki.print((typeof r.fuel === "function" ? r.fuel() : r.fuel));
(typeof r.lift_off === "function" ? r.lift_off() : r.lift_off);