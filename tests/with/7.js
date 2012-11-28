function f(x) {
    var newObject = {
        "a": 1,
        "b": 2,
        "c": 3,
        "q": 4
    };
    var newObject2 = {
        "z" : 5
    };
    with (newObject, newObject2) {
        return z;
    }
}
395, f(395);