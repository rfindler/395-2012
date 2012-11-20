(function() {
    var firstObject = {
        "a": 1,
        "b": 2,
        "c": 3,
        "q": 4
    };
    var f;
    with (firstObject) {
        f = function(x) {
            q = x;
        }
    };
    firstObject.q = 17;
    var g;
    with (firstObject) {
        g = function(x) {
            return q;
        }
    }
    f(395);
    return g(2);
})()