function f(x) {
    var newObject = {
        "a": 1,
        "b": 2,
        "c": 3
    };
    with (newObject) {
        var q = 3;
    }
    return q;
}
f(395);