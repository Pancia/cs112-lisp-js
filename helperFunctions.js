var loki = (function (){
    var loki = {};

    loki.print = function() {
        var _log = function(x) {
            if (typeof(console) === "object")
            {console.log(x);} else {print(x);}
        };
        var args = sliceArgs(arguments);
        args.forEach(_log);
    };

    loki.get = function(e, i) {return e[i];};

    var sliceArgs = function (args) {
        return args.length > 0 ? [].slice.call(args, 0) : [];
    };

    var foldl = curry(function (iterator, cumulate, items) {
        iterator = checkFunction(iterator);
        fjs.each(function (item, i) {
            cumulate = iterator.call(null, cumulate, item, i);
        }, items);
        return cumulate;
    });


    var curry = function (fn) {
        if (!typeof(fn) !== "function")
        {throw "loki error: curry expected a function"}
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

    loki.plus  = loki.curry(function(x, y) {return x + y});
    loki.minus = loki.curry(function(x, y) {return x - y});
    loki.mult  = loki.curry(function(x, y) {return x * y});
    loki.div   = loki.curry(function(x, y) {return x / y});

    loki.and = loki.curry(function(x, y) {return x && y});
    loki.or  = loki.curry(function(x, y) {return x || y});
    loki.eq  = loki.curry(function(x, y) {return x == y});
    loki.neq = loki.curry(function(x, y) {return x != y});
    loki.lt  = loki.curry(function(x, y) {return x < y});
    loki.lte = loki.curry(function(x, y) {return x <= y});
    loki.gt  = loki.curry(function(x, y) {return x > y});
    loki.gte = loki.curry(function(x, y) {return x >= y});

    return loki;
})();
//END HELPER FUNCTIONS
