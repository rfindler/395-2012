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
            return q;
        }
    };
    firstObject.q = 17;
    return f(395);
})()