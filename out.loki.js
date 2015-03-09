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
    var eq = curry(function(x, y) {return (x === y ? x : false)});
    loki.eq  = function(x,y) {return !!eq(x,y);}
    loki.neq  = function(x,y) {return !eq(x,y);}
    var lt = curry(function(x, y) {return (x < y ? y : false)});
    loki.lt  = function(x,y) {return !!lt(x,y);}
    var lte = curry(function(x, y) {return (x <= y ? y : false)});
    loki.lte  = function(x,y) {return !!lte(x,y);}
    var gt = curry(function(x, y) {return (x > y ? y : false)});
    loki.gt  = function(x,y) {return !!gt(x,y);}
    var gte = curry(function(x, y) {return (x >= y ? y : false)});
    loki.gte  = function(x,y) {return !!gte(x,y);}

    return loki;
})();
//END LOKI HELPER FUNCTIONS
MyObject.prototype.constructor = MyObject;
function MyObject() {
;
this.obj = 5
};
MyObject.prototype.get_object = function() {
 return "object"
};;
var o = new MyObject();
loki.print((typeof o.get_object === "function" ? o.get_object() : o.get_object));
Thing.prototype.constructor = Thing;
function Thing() {
;
this.thing = 5
};
Thing.prototype.get_thing = function() {
 return "thing"
};;
var t = new Thing();
loki.print((typeof t.get_thing === "function" ? t.get_thing() : t.get_thing));
loki.extend(Rocket.prototype, MyObject.prototype);
loki.extend(Rocket.prototype, Thing.prototype);
Rocket.prototype.constructor = Rocket;
function Rocket(x) {
this.speed = 5;
this.p = x;
this.f = 0
};
Rocket.prototype.fe = function() {
 return loki.print("ehhhh")
};
Rocket.prototype.funct = function(x, y) {
 return loki.print(loki.plus(x, y))
};;
var r = new Rocket(5);
loki.print((typeof r.f === "function" ? r.f() : r.f));
loki.print((typeof r.get_object === "function" ? r.get_object() : r.get_object));
loki.print((typeof r.get_thing === "function" ? r.get_thing() : r.get_thing));
for (lskdfjk in loki.range(2)){
for (xyz in loki.range(4)){
for (abc in loki.range(7)){
for (lskdfjl in loki.range(2)){
loki.print(xyz)
}
}
}
};