var log = function() {
    var f = function(x) {print(x)}
    var args = Array.prototype.slice.call(arguments);
    args.forEach(f);
}
