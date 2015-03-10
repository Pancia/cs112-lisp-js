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
function MyObject(x) {
;
this.obj = x
};
MyObject.prototype.get_obj = function() {
 return (typeof this.obj === "function" ? this.obj() : this.obj)
};;
MyThing.prototype.constructor = MyThing;
function MyThing(x) {
;
this.thing = x
};
MyThing.prototype.get_thing = function() {
 return (typeof this.thing === "function" ? this.thing() : this.thing)
};;
loki.extend(Rocket.prototype, MyObject.prototype);
loki.extend(Rocket.prototype, MyThing.prototype);
Rocket.prototype.constructor = Rocket;
function Rocket(x) {
this.color = "red";
this.fuel = 7;
MyObject.call(this,"obj");
MyThing.call(this,"thing");
this.speed = x;
loki.print("eval-in-constr");
this.foo = "foo";
for (x in loki.range(10)){
loki.print(x)
}
};
Rocket.prototype.lift_off = function() {
 return loki.print(loki.plus("I'm flying @ ", (typeof this.speed === "function" ? this.speed() : this.speed), " speed"))
};
Rocket.prototype.toString = function() {
 return loki.plus("I'm a ", (typeof this.color === "function" ? this.color() : this.color), " rocket")
};;
var r = new Rocket("5");
loki.print((typeof r.foo === "function" ? r.foo() : r.foo));
loki.print((typeof r.color === "function" ? r.color() : r.color));
loki.print((typeof r.speed === "function" ? r.speed() : r.speed));
loki.print((typeof r.toString === "function" ? r.toString() : r.toString));
loki.print((typeof r.fuel === "function" ? r.fuel() : r.fuel));
(typeof r.lift_off === "function" ? r.lift_off() : r.lift_off);
loki.print((typeof r.get_obj === "function" ? r.get_obj() : r.get_obj));
loki.print((typeof r.thing === "function" ? r.thing() : r.thing));